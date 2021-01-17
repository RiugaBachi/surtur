-- | Exports the @Core@ type and constraint type families to restrict core versioning in functions. By convention, surtur will set the maximum version for a core API function starting at the minimum version where it is deprecated, not necessarily obsolete. This is a safety precaution as surtur is a high level API which may break under the unpredictability of mixed usage with deprecated Vulkan API---a phenomenon that has been observed under the deprecation of fixed functions in OpenGL 3.0.
module Surtur.Instance.CoreVersioning (
  Core,
  Min,
  Max
) where

import Data.Type.Bool
import Data.Type.Equality
import Data.Kind
import GHC.TypeNats
import GHC.TypeLits
import Type.Errors.Pretty

data Core (maj :: Nat) (min :: Nat)

type family Min a b where 
  a `Min` b = 
    If ((a :>= b) == 'True) 
      (() :: Constraint)
      (TypeError (MinError a b))

type MinError a b 
  = "Minimum Vulkan core version not met."
  % "  Required: >= " <> b
  % "  Context: " <> a 

type family Max a b where 
  a `Max` b = 
    If ((a :<= b) == 'True) 
      (() :: Constraint)
      (TypeError (MaxError a b))

type MaxError a b
  = "Exceeded maximum Vulkan core version."
  % "  Required: <= " <> b
  % "  Context: " <> a

type family a :>= b where
  (Core maj min) :>= (Core maj' min') = 
    (maj `CmpNat` maj' == 'GT) || 
      ((maj == maj') && (Not (min `CmpNat` min' == 'LT)))
  _ :>= _ = 
    TypeError 
      ('Text "Version comparison can only take place between two Core types.")

type family a :<= b where
  (Core maj min) :<= (Core maj' min') = 
    (maj `CmpNat` maj' == 'LT) || 
      ((maj == maj') && (Not (min `CmpNat` min' == 'GT)))
  _ :<= _ = 
    TypeError 
      ('Text "Version comparison can only take place between two Core types.")
