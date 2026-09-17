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
        && not (isStrict (exprType e2)) && not (isDeepseqForce e2) && not (isLit e2)
        -- since GHC 9.14 the HasCallStack dictionary is built by
        -- applying the IP constructor to a 'pushCallStack' call, so the
        -- call stack plumbing turns up in argument position as well
        && not (isPushCallStack e2))
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


isPushCallStack :: CoreExpr -> Bool
isPushCallStack (Var v) =
  case getNameModule v of
    Just (name, mod) -> mod == "GHC.Stack.Types" && name == "pushCallStack"
    _ -> False
isPushCallStack (App x _) = isPushCallStack x
isPushCallStack _ = False

-- | Check whether the given expression is in head position of an
-- application whose arguments should not be checked for
-- strictness. This covers the desugaring of @OverloadedLists@
-- ('fromList', 'fromListN') and @OverloadedStrings@ ('fromString'),
-- the construction of 'Data.Text.Text' literals, and the call stack
-- plumbing for 'GHC.Stack.HasCallStack'. In each of these cases the
-- lazy argument is immediately consumed by a function that we know
-- does not retain it, so it cannot cause a space leak.
--
-- Note that the module names below are the ones after normalisation
-- by 'baseModuleName', which maps the @GHC.Internal.*@ modules that
-- GHC 9.10 and later use back to their pre-9.10 names.
ignoreArgument :: CoreExpr -> Bool
ignoreArgument (Var v) =
  case getNameModule v of
    Just (name, mod) ->
      ((mod == "GHC.Exts" || mod == "GHC.IsList") && (name == "fromList" || name == "fromListN")) ||
      ((mod == "Data.String" || mod == "GHC.Data.String") && name == "fromString") ||
      (mod == "GHC.Stack.Types" && name == "pushCallStack") ||
      ((mod == "Data.Text" || mod == "Data.Text.Internal") && name == "pack")
    _ -> False
ignoreArgument (App x _) = ignoreArgument x
ignoreArgument _ = False

isDeepseqForce :: CoreExpr -> Bool
isDeepseqForce (App (App (App (Var v) _) _) _) =
  case getNameModule v of
    Just (name, mod) -> mod == "Control.DeepSeq" && name == "force"
    _ -> False
isDeepseqForce _ = False
