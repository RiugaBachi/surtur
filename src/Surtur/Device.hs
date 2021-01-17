{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Surtur.Device (
) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text ( Text )
import Data.Kind
import Data.Maybe
import Data.List
import Data.HList.HList
import Data.Singletons
import Data.Singletons.TH hiding ( Min )
import Data.Singletons.TH.Options
import qualified Data.Vector as V
import Control.Monad.Trans.MultiReader
import Control.Monad.Managed
import Vulkan.Zero
import Vulkan.Core10 hiding ( Filter )
import Vulkan.Core10.DeviceInitialization
import Surtur.Instance
import Surtur.Instance.CoreVersioning
import Surtur.Instance.Extensions
import Surtur.Internal.Proof
import Surtur.TH

data ImageProperties
  = ImageProperties Format ImageType ImageTiling ImageUsageFlags ImageCreateFlags

-- * Claims

mkClaimSet ''PhysicalDeviceFeatures
mkClaimSings ''PhysicalDeviceFeaturesClaim
mkClaimBijections ''PhysicalDeviceFeatures
mkLiftedConSet ''PhysicalDeviceFeaturesClaim

-- * Physical device

type Score = Int

type PhysicalDevicePicker m r s p
  = Proof PhysicalDevice (m r s) (Score, HList p)

-- | Regarding proofs, see:
-- (Core)
-- * 'PhysicalDeviceFeatures'
-- * 'PhysicalDeviceProperties'
-- * 'PhysicalDeviceMemoryProperties'
-- * 'FormatProperties'
-- * 'ImageFormatProperties'
-- * ['QueueFamilyProperties']
-- (Ext/VK_KHR_surface)
-- * 'SurfaceCapabilitiesKHR'
-- 
-- Type variables:
-- @v@ - 'Instance' core version
-- @e@ - 'Instance'-level required extensions 
-- @r@ - Resources in the stack
-- @u@ - Logical device -level required extensions
-- @p@ - 'PhysicalDevice' guarantees and claims
-- @s@ - Critical monad scope
-- @m@ - MonadVulkan instance
withPhysicalDevice
  :: forall v e r (p :: [Type]) s m.
      ( Min v (Core 1 0)
      , MonadVulkan v e m
      , Has Instance r
      )
  => PhysicalDevicePicker m r s p
  -> m ((PhysicalDevice ': p) :++: r) s Result
  -> m r s Result
withPhysicalDevice pick cont = do
  (res, (V.toList -> devs)) <- enumeratePhysicalDevices =<< grab @Instance
  case res of
    SUCCESS -> do
      Just (physDev, (_, proofs)) <- 
        safeHead 
          . maximumBy cmp 
          . zip devs 
          . catMaybes
         <$> mapM (flip runProof pick) devs
      withResources @p proofs $ withResource physDev $ cont
    x -> pure x
  where
    safeHead (null -> True) = Nothing
    safeHeader v = Just $ head v

    cmp (fst . snd -> x) (fst . snd -> y) = compare x y

-- * Logical device

type DeviceExtension = Text

-- | A wrapper around 'Device' carrying with it a type-level list of required
-- device-level extensions @d@.
newtype LogicalDevice d
  = LogicalDevice { unwrapHandle :: Device }

-- | Enabling a subset of the features supported by the parent PhysicalDevice,
-- although possible, is at this time not supported.
--
-- Regarding proofs, see:
-- * 'getDeviceQueue'
-- 
-- Type variables:
-- @v@ - 'Instance' core version
-- @e@ - 'Instance'-level required extensions 
-- @r@ - Resources in the stack
-- @u@ - Logical device -level required extensions
-- @p@ - 'PhysicalDevice' guarantees and claims
-- @s@ - Critical monad scope
-- @m@ - MonadVulkan instance
withLogicalDevice
  :: forall v e d r (p :: [Type]) s m g h.
      ( Min v (Core 1 0)
      , MonadVulkan v e m
      , MonadManaged (m r s)
      , Has Instance r
      , Has (Guaranteed PhysicalDevice g) r
      , Stringify d
      )
  => [DeviceExtension]
  -- ^ Optional device-level extensions
  -> Guaranteed PhysicalDeviceFeatures h
  -> Proof (LogicalDevice d) (m r s) (HList p)
  -> m ((LogicalDevice d ': p) :++: r) s Result
  -> m r s Result
withLogicalDevice optExts _ proof cont = do
  (Guaranteed physDev) <- grab @(Guaranteed PhysicalDevice g)
  logDev <- LogicalDevice <$> withDevice physDev (deviceCreateInfo) Nothing allocate
  Just proofs <- runProof logDev proof
  withResources @p proofs . withResource logDev $ cont
  where
    deviceCreateInfo =
      zero { queueCreateInfos = 
              V.fromList []
           , enabledExtensionNames = 
              V.fromList $ fmap T.encodeUtf8 $ (T.pack <$> stringify (Proxy @d)) <> optExts
           }
