{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module AsyncRattus.Plugin.Strictify
  (checkStrictData, SCxt (..)) where
import Prelude hiding ((<>))
import Control.Monad
import AsyncRattus.Plugin.Utils

import GHC.Plugins
import GHC.Types.Tickish

data SCxt = SCxt {srcSpan :: SrcSpan}

-- | Checks whether the given expression uses non-strict data types
-- and issues a warning if it finds any such use.
checkStrictData :: SCxt -> CoreExpr -> CoreM ()
checkStrictData ss (Let (NonRec _ e1) e2) = 
  checkStrictData ss e1 >> checkStrictData ss e2
checkStrictData ss (Case e _ _ alts) = do
  checkStrictData ss e
  mapM_ ((\(_,_,e) ->  checkStrictData ss e) . getAlt) alts
checkStrictData ss (Let (Rec es) e) = do
  mapM_ (\ (_,e) -> checkStrictData ss e) es
  checkStrictData ss e
checkStrictData ss (Lam _ e) = checkStrictData ss e
checkStrictData ss (Cast e _) = checkStrictData ss e
checkStrictData ss (Tick (SourceNote span _) e) = 
  checkStrictData (ss{srcSpan = fromRealSrcSpan span}) e
checkStrictData ss (App e1 e2)
  | isPushCallStack e1 = return ()
  | isFromList e1 = return ()
  | isFromString e1 = return ()
  | otherwise = do 
    when (not (isType e2) && tcIsLiftedTypeKind(typeKind (exprType e2))
        && not (isStrict (exprType e2)) && not (isDeepseqForce e2) && not (isLit e2))
          (printMessage SevWarning (srcSpan ss)
               (text "The use of lazy type " <> ppr (exprType e2) <> " may lead to memory leaks. Use Control.DeepSeq.force on lazy types."))
    checkStrictData ss e1
    checkStrictData ss e2
checkStrictData _ss _ = return ()

isLit :: CoreExpr -> Bool
isLit Lit{} = True
isLit (App (Var v) Lit{}) 
  | Just (name,mod) <- getNameModule v = mod == "GHC.CString" && name == "unpackCString#"
isLit _ = False


isFromList :: CoreExpr -> Bool
isFromList (Var v) =
  case getNameModule v of
    Just (name, mod) -> (mod == "GHC.Exts" || mod == "GHC.IsList") && (name == "fromList" || name == "fromListN")
    _ -> False
isFromList (App x _) = isFromList x
isFromList _ = False

isFromString :: CoreExpr -> Bool
isFromString (Var v) =
  case getNameModule v of
    Just (name, mod) -> mod == "Data.String" && name == "fromString"
    _ -> False
isFromString (App x _) = isFromString x
isFromString _ = False

isPushCallStack :: CoreExpr -> Bool
isPushCallStack (Var v) =
  case getNameModule v of
    Just (name, mod) -> mod == "GHC.Stack.Types" && name == "pushCallStack"
    _ -> False
isPushCallStack (App x _) = isPushCallStack x
isPushCallStack _ = False

isDeepseqForce :: CoreExpr -> Bool
isDeepseqForce (App (App (App (Var v) _) _) _) =
  case getNameModule v of
    Just (name, mod) -> mod == "Control.DeepSeq" && name == "force"
    _ -> False
isDeepseqForce _ = False
