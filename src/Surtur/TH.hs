module Surtur.TH (
  mkLiftedConSet,
  mkClaimSet,
  mkClaimBijections,
  mkClaimSings
) where

import Data.List.Extra
import Data.Char
import Data.Singletons.TH hiding ( Min )
import Data.Singletons.TH.Options
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Surtur.Proof

capitalize :: String -> String
capitalize [] = []
capitalize (f:r) = toUpper f : r

mkLiftedConSet :: Name -> Q [Dec]
mkLiftedConSet n = do
  gadt <- reifyDatatype n
  let ctors = constructorName <$> datatypeCons gadt
  let tySynName = mkName $ nameBase (datatypeName gadt) <> "ValSet"
  pure . pure $ TySynD tySynName [] $ 
    foldr (\cn -> InfixT (PromotedT cn) (mkName ":+>")) (ConT (mkName "KPNil")) (reverse ctors)

mkClaimSet :: Name -> Q [Dec]
mkClaimSet n = do
  dt <- reifyDatatype n
  let ctor = head $ datatypeCons dt
  let tys = constructorFields ctor
  RecordConstructor cns <- pure . constructorVariant $ ctor
  let gadtName = mkName $ nameBase (datatypeName dt) <> "Claim"
  let gadt = gadtDecl (nameBase <$> cns) tys $ gadtName
  pure [gadt]
  where
    gadtDecl (fmap capitalize -> cns) tys gadtName =
      DataD [] gadtName [PlainTV $ mkName "a"] Nothing 
        ((\(u, t) -> GadtC [mkName u] [] (AppT (ConT gadtName) t)) <$> zip cns tys) []

mkClaimBijections :: Name -> Q [Dec]
mkClaimBijections n = do
  dt <- reifyDatatype n
  let ctor = head $ datatypeCons dt
  RecordConstructor cns <- pure . constructorVariant $ ctor
  let gadtName = mkName $ nameBase (datatypeName dt) <> "Claim"
  let liftedRepTyInst = TySynInstD $ 
        TySynEqn Nothing 
          (AppT (ConT (mkName "Lifted")) (ConT gadtName))
          (ConT $ mkName $ "S" <> nameBase gadtName)
  let claimBijectionDecls = FunD (mkName "claim") $ flip fmap cns $ \cn ->
        Clause 
          [ConP (mkName ('S' : capitalize (nameBase cn))) []] 
          (NormalB (VarE cn))
          []
  let instDecls = liftedRepTyInst : claimBijectionDecls : []
  let inst = InstanceD Nothing [] 
        (AppT (AppT 
          (ConT (mkName "ClaimSet"))
          (ConT (mkName (dropEnd 5 (nameBase gadtName))))) 
            (ConT gadtName))
              instDecls
  pure [inst]

mkClaimSings :: Name -> Q [Dec]
mkClaimSings =
  withOptions defaultOptions { genSingKindInsts = False } .
    genSingletons . pure
