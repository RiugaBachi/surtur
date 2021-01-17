module Surtur.Instance.Extensions (
  Stringify,
  stringify,
  ExtensionError,
  Required,
  Requirement,
  Requirements
) where

import Data.Proxy
import Data.Kind
import GHC.TypeLits
import Type.Errors.Pretty

class Stringify a where
  stringify :: Proxy a -> [String]

instance Stringify '[] where
  stringify _ = []

instance (KnownSymbol x, Stringify xs) => Stringify (x ': xs) where
  stringify _ = symbolVal (Proxy @x) : stringify (Proxy @xs)

type family Required (e :: k) (es :: [k]) :: Bool where
  Required e '[] = 'False
  Required e (e:es) = 'True
  Required e (x:es) = Required e es

type ExtensionError e
  = "Necessary Vulkan extension not loaded: " <> e

type family Requirement (e :: k) (es :: [k]) :: Constraint where
  Requirement e '[] = TypeError (ExtensionError e)
  Requirement e (e:es) = ()
  Requirement e (x:es) = Requirement e es

type family Requirements (es :: [k]) (es' :: [k]) :: Constraint where
  Requirements _ '[] = ()
  Requirements es (e:es') = (Requirement e es, Requirements es es')
