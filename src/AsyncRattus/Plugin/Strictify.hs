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
  | ignoreArgument e1 = return ()
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


ignoreArgument :: CoreExpr -> Bool
ignoreArgument (Var v) =
  case getNameModule v of
    Just (name, mod) -> 
      ((mod == "GHC.Exts" || mod == "GHC.IsList") && (name == "fromList" || name == "fromListN")) ||
      (mod == "Data.String" && name == "fromString") ||
      (mod == "GHC.Stack.Types" && name == "pushCallStack") ||
      (mod == "Data.Text.Internal" && name == "pack")
    _ -> False
ignoreArgument (App x _) = ignoreArgument x
ignoreArgument _ = False

isDeepseqForce :: CoreExpr -> Bool
isDeepseqForce (App (App (App (Var v) _) _) _) =
  case getNameModule v of
    Just (name, mod) -> mod == "Control.DeepSeq" && name == "force"
    _ -> False
isDeepseqForce _ = False
