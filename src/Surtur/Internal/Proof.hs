module Surtur.Internal.Proof (
  KPNil,
  (:+>),
  HoldsForall,
  HoldsForall',
  Filter,
  Proof(..),
  runProof,
  subproof,
  inspect,
  ClaimSet,
  Lifted,
  claim,
  Claim(..),
  Guarantee,
  Guaranteed(..),
  assume,
  ensure
) where

import Data.Bool
import Data.Type.Bool
import Data.Kind
import Data.Singletons
import Control.Monad.Trans.Class
import Control.Monad.Reader
import Control.Monad.Except

-- * Kind-heterogenous typ-level lists

-- | Nil type for the type-level cons operator (:+>)
data KPNil

-- | Kind-heteregenous type-level cons operator
data (a :: t) :+> (b :: k)

-- | Filters all elements of kind @t@ into a type-level heterogenous list of kind @[t]@
type family FilterKPL t a where
  FilterKPL t KPNil = '[]
  FilterKPL t ((a :: t) :+> b) = t ': FilterKPL t b

-- * Basic Propositions

type HoldsForall p xs = HoldsForall' p xs ~ 'True

type family HoldsForall' p xs where
  HoldsForall' _ '[] = 'True
  HoldsForall' p (x:xs) = p x && HoldsForall' p xs

type family Filter p xs where
  Filter _ '[] = '[]
  Filter p (x:xs) = If (p x) (x : Filter p xs) (Filter p xs)

-- * Proofs

-- | A proof regarding the fields of a structure @g@; evaluates to Nothing if any one 
-- proposition fails, and allows extractable type-level witnesses to the proof in the 
-- form of 'Guarantee's should it succeed.
newtype Proof g m a
  = Proof { unwrapProof :: ReaderT g (ExceptT () m) a }
  deriving newtype 
    ( Functor
    , Applicative
    , Monad
    , MonadError ()
    )

instance MonadTrans (Proof g) where
  lift = Proof . lift . lift
    
-- | Perhaps more aptly named "qed".
runProof :: Monad m => g -> Proof g m a -> m (Maybe a)
runProof x = 
  fmap (either (const Nothing) Just) 
    . runExceptT 
    . flip runReaderT x 
    . unwrapProof

-- | Zoom into a sub-structure @g'@ of  parent structure @g@ and perform a proof
-- strictly on this sub-structure.
subproof :: (g -> g') -> Proof g' m a -> Proof g m a
subproof f (Proof p) = Proof (withReaderT f p)

-- | Given a claim representative @x@, extract the associated claim from parent structure @g@.
inspect 
  :: forall c t (x :: c t) g m. 
      ( Monad m
      , ClaimSet g c
      , Sing x ~ Lifted c x
      , SingI x
      ) 
  => Proof g m (Claim x)
inspect = Proof . fmap Claim . asks . claim $ sing @x

-- | A guarantee placed on the structure field of type @t@ in a bijection with
-- its claim representative @x@ belonging to claim set @c@ and associated with
-- parent structure @g@. The guarantee acts as a type-level witness that said field 
-- obeys the proposition @r@; that is to say @r k ~ 'True@ forall @(k :: t)@
data Guarantee x (r :: k -> Bool)
  = Guarantee

data Guaranteed x (r :: [Type])
  = Guaranteed x

-- | Conventionally, a claim set on parent structure @g@ is a GADT for which each 
-- member forms a bijection with the fields of @g@.
class ClaimSet g (c :: Type -> Type) | g -> c, c -> g where
  type Lifted c :: forall a. c a -> Type
  claim :: forall t (v :: c t). Lifted c v -> g -> t

-- | A claim on parent structure @g@ identified by some type-level claim representative @x@
-- belonging to claim set @c@ in a bijection with @g@. Unlike a guarantee, a claim has no 
-- type-level guarantees and must be checked at runtime.
data Claim (x :: c (t :: Type))
  = Claim t

-- | Locally elevate a claim to a guarantee under the assumption that it is so.
-- Should the guarantee's arbitrary proposition @h@ fail to be proven at runtime, 
-- the function dependent upon the assumption will simply not be evaluated. 
-- This effectively provides branching on Claims post-proof for functions constrained 
-- under Guarantees.
assume
  :: forall k (h :: k -> Bool) r c t (x :: c t).
      ( SingKind k
      , t ~ Demote k
      , forall a. SingI (h a)
      ) 
  => Claim x 
  -- ^ The claim to be elevated as a guarantee by assumption.
  -> (Guarantee x h -> r)
  -- ^ The function to be evaluated under the assumption that the guarantee holds.
  -> r
  -- ^ The alternative function to be evaluated should the assumption be unsound (i.e. false).
  -> r
assume (Claim v) f g =
  withSomeSing @k v go
  where
    go :: forall a. Sing (a :: k) -> r
    go _ = bool g (f Guarantee) $ demote @(h a)

-- | Elevate a claim to a guarantee, at the risk of the proof failing should the claim be false.
ensure 
  :: forall k (h :: k -> Bool) c t (x :: c t) g m.
      ( Monad m
      , SingKind k
      , t ~ Demote k
      , forall a. SingI (h a)
      ) 
  => Claim x 
  -> Proof g m (Guarantee x h)
ensure c = assume c pure $ throwError ()
