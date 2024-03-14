{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}


module AsyncRattus.Derive (continuous) where

import AsyncRattus.InternalPrimitives
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad


data DataInfo = forall flag . DataInfo Cxt Name [TyVarBndr flag] [Con] [DerivClause] 

{-|
  This function provides a list (of the given length) of new names based
  on the given string.
-}
newNames :: Int -> String -> Q [Name]
newNames n name = replicateM n (newName name)


{-|
  This is the @Q@-lifted version of 'abstractNewtype.
-}
abstractNewtypeQ :: Q Info -> Q (Maybe DataInfo)
abstractNewtypeQ = liftM abstractNewtype


{-| Apply a class name to type arguments to construct a type class
    constraint.
-}

mkClassP :: Name -> [Type] -> Type
mkClassP name = foldl AppT (ConT name)


{-| This function provides the name and the arity of the given data
constructor, and if it is a GADT also its type.
-}
normalCon :: Con -> [(Name,[StrictType], Maybe Type)]
normalCon (NormalC constr args) = [(constr, args, Nothing)]
normalCon (RecC constr args) = [(constr, map (\(_,s,t) -> (s,t)) args, Nothing)]
normalCon (InfixC a constr b) = [(constr, [a,b], Nothing)]
normalCon (ForallC _ _ constr) = normalCon constr
normalCon (GadtC (constr:_) args typ) = [(constr,args,Just typ)]
normalCon (RecGadtC (constr : _) args typ) = [(constr,map dropFst args,Just typ)]
  where dropFst (_,x,y) = (x,y)
normalCon _ = error "missing case for 'normalCon'"

normalCon' :: Con -> [(Name,[Type], Maybe Type)]
normalCon' con = map conv (normalCon con)
  where conv (n, ts, t) = (n, map snd ts, t)
  
mkInstanceD :: Cxt -> Type -> [Dec] -> Dec
mkInstanceD cxt ty decs = InstanceD Nothing cxt ty decs

{-|
  This function returns the name of a bound type variable
-}
tyVarBndrName (PlainTV n _) = n
tyVarBndrName (KindedTV n _ _) = n

{-|
  This function abstracts away @newtype@ declaration, it turns them into
  @data@ declarations.
-}
abstractNewtype :: Info -> Maybe DataInfo
abstractNewtype (TyConI (NewtypeD cxt name args _ constr derive))
    = Just (DataInfo cxt name args [constr] derive)
abstractNewtype (TyConI (DataD cxt name args _ constrs derive))
    = Just (DataInfo cxt name args constrs derive)
abstractNewtype _ = Nothing

continuous :: Name -> Q [Dec]
continuous fname = do
  Just (DataInfo _cxt name args constrs _deriving) <- abstractNewtypeQ $ reify fname
  let argNames = map (VarT . tyVarBndrName) args
      complType = foldl AppT (ConT name) argNames
      preCond = map (mkClassP ''Continuous . (: [])) argNames
      classType = AppT (ConT ''Continuous) complType
  let constrs' = concatMap normalCon' constrs
  promDecl <- funD 'progressInternal (promClauses constrs')
  return [mkInstanceD preCond classType [promDecl]]
      where promClauses = map genPromClause
            genPromClause (constr, args,_) = do
              let n = length args
              varNs <- newNames n "x"
              varIn <- newName "_inp"
              let pat = ConP constr [] $ map VarP varNs
                  allVars = map varE varNs
                  inpVar = varE varIn
              body <- appsE ( conE constr : (map (\ x -> [|progressInternal $inpVar $x|]) allVars))
              return $ Clause [VarP varIn, pat] (NormalB body) []