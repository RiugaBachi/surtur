module Surtur.Instance (
  (:++:),
  allocate,
  (.&&.),
  Has,
  Requirable(..),
  Vulkan,
  withResource,
  withResources,
  grab,
  runVulkan,
  MonadVulkan,
  liftVK
) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Vector as V
import Data.Bits
import Data.Word
import Data.List
import Data.HList.ContainsType
import Data.HList.HList
import Data.Singletons
import Data.Singletons.Prelude ( SBool(..) )
import Control.Monad.Critical
import Control.Monad.State
import Control.Monad.Trans.MultiReader
import Control.Monad.Managed
import Control.Exception
import GHC.TypeLits
import Vulkan.Core10
import Vulkan.Core11
import Vulkan.Core12
import Vulkan.Zero
import Vulkan.Exception
import Surtur.Instance.Extensions
import Surtur.Instance.CoreVersioning

type (:++:) x y = x `Append` y
type Has = ContainsType

allocate :: MonadManaged m => IO a -> (a -> IO ()) -> m a
allocate c d = using $ managed (bracket c d)

(.&&.) :: Bits a => a -> a -> Bool
u .&&. v = (/= zeroBits) (u .&. v)

deriving instance MonadFail m => MonadFail (MultiReaderT r m)
deriving instance MonadManaged m => MonadManaged (MultiReaderT r m)

newtype Vulkan v e r s a
  = Vulkan { unVulkan :: MultiReaderT r (Critical s) a }
  deriving newtype 
    ( Functor
    , Applicative
    , Monad 
    , MonadFail
    , MonadIO
    , MonadManaged
    )

instance MonadCritical (Vulkan v e r) where
  critically x = Vulkan $ lift x

class ( forall r s. MonadIO (m r s)
      , forall r s. MonadFail (m r s)
      ) => MonadVulkan v e m | m -> v e where
  liftVK 
    :: Vulkan v e r s a 
    -> m r s a
  unliftVK
    :: m r s a
    -> Vulkan v e r s a

instance MonadVulkan v e (Vulkan v e) where
  liftVK = id

withResource
  :: forall p r v e m s a.
      ( MonadVulkan v e m 
      )
  => p
  -> m (p ': r) s a
  -> m r s a
withResource r (unliftVK -> Vulkan vk) = 
  liftVK . Vulkan $ withMultiReader r vk

withResources
  :: forall p r v e m s a.
      ( MonadVulkan v e m
      )
  => HList p
  -> m (p :++: r) s a
  -> m r s a
withResources rs (unliftVK -> Vulkan vk) = 
  liftVK . Vulkan $ withMultiReaders rs vk

-- | Grab a resource from the internal Vulkan resource stack.
grab
  :: forall a r v e m s.
      ( MonadVulkan v e m
      , Has a r
      ) 
  => m r s a 
grab = liftVK $ Vulkan mAsk

-- | Guard against the usage of a /non-required/ extension in the event
-- that it was unsupported and not loaded upon instance creation,
-- providing an alternative codepath that does not require said
-- extension.
--
-- 'guardVK' is designed to be chained together to produce a form of
-- type-safe branching which assume different /non-required/ extensions
-- each time.
guardVK
  :: forall v e m s a r i (o :: Symbol). 
      ( MonadVulkan v e m
      , SingI i
      , i ~ Required o e
      )
  => (forall e'. Vulkan v e' r s a)
  -> m r s a
  -> m r s a
guardVK (Vulkan vk) fallback = 
  case sing @i of
    STrue -> liftVK (Vulkan vk :: Vulkan v e r s a)
    SFalse -> fallback

type AppName = Text
type AppVersion = Word32
type Extension = Text
type Layer = Text

data Requirable a
  = Required a
  | Optional a
  deriving (Functor)

required :: [Requirable a] -> [a]
required [] = []
required (Optional x : xs) = x : required xs
required (_ : xs) = required xs

optional :: [Requirable a] -> [a]
optional [] = []
optional (Optional x : xs) = x : optional xs
optional (_ : xs) = optional xs

-- | Create and run a Vulkan instance.
--
-- 'runVulkan' will attempt to load as many of the specified /non-required/
-- extensions as possible. On the other hand, all type-level /required/ extensions 
-- will be forcefully loaded, resulting in an early exit with an 'ERROR_EXTENSION_NOT_PRESENT' 
-- result should any one of them not be supported by the driver.
--
-- Similarly, all 'Required' layers will be forcefully loaded, resulting in an early exit
-- with the 'ERROR_LAYER_NOT_PRESENT' result should any one layer not be supported by the
-- driver. As many @Optional@ layers will be loaded as possible. Unlike extensions,
-- required layers are not defined at the type level as there is little to no benefit
-- in terms of type-safety to be attained from such a design.
runVulkan 
  :: forall v e (maj :: Nat) (min :: Nat). 
      ( v ~ Core maj min
      , Min v (Core 1 0)
      , Max v (Core 1 2)
      , KnownNat maj
      , KnownNat min
      , Stringify e
      )
  => Maybe AppName
  -> Maybe AppVersion
  -> [Extension]
  -> [Requirable Layer]
  -> (forall s. Vulkan v e '[Instance] s ()) 
  -> IO Result
runVulkan appName appVersion optExts (fmap (fmap encodeUtf8) -> layers) vk = do 
  (res, (V.toList . fmap layerName -> supportedLayers)) 
    <- enumerateInstanceLayerProperties
  case res of
    SUCCESS -> fmap (either vulkanExceptionResult (const SUCCESS)) . try $ runCritical $ do
      inst <- withInstance (instanceCreateInfo supportedLayers) Nothing allocate
      runMultiReaderTNil_ . withMultiReader_ inst $ unVulkan vk
    _ -> pure res
  where
    toVersion 1 0 = API_VERSION_1_0 
    toVersion 1 1 = API_VERSION_1_1 
    toVersion 1 2 = API_VERSION_1_2 
    toVersion _ _ = undefined
    
    instanceCreateInfo supportedLayers =
      zero { applicationInfo = Just $
              zero { applicationName = encodeUtf8 <$> appName
                   , applicationVersion = maybe 0 id appVersion
                   , engineName = Just "surtur"
                   , apiVersion = toVersion (natVal (Proxy @maj)) (natVal (Proxy @min))
                   }
           , enabledLayerNames = V.fromList $ 
              required layers <> optional layers `intersect` supportedLayers
           , enabledExtensionNames = V.fromList . fmap encodeUtf8 $
              (T.pack <$> stringify (Proxy @e)) <> optExts
           }

