module LskFileHandler where

import DynFlags
import LskToHs
import HeaderInfo
import HsSyn
import LskTransformationMonad
import SrcLoc
import FastString
import Util
import Module
import Data.List
import StringBuffer
import HscTypes
import ErrUtils
import System.FilePath
import PrelNames (gHC_PRIM)

getImportsLsk :: DynFlags -> StringBuffer -> FilePath -> FilePath
    -> IO ([Located ModuleName], [Located ModuleName], Located ModuleName)

getImportsLsk dflags buf filename source_filename =
  if isLiskellSrcFilename filename then
      getImportsLsk' dflags buf filename source_filename
  else 
      getImports dflags buf filename source_filename 

getImportsLsk' dflags buf filename source_filename =
  do
    env <- seedLskTrfEnv
    module_t <- runTM (liskell_transform_header_only buf (mkSrcLoc (mkFastString filename) 1 0)) 
		(TransformationState env (error "Importing should not touch hsc_env") (newFreshVarStream "parse") ([],[]))
    case module_t of
      Left (TrErr s m) -> parseError s m
      Right (_, rdr_module) -> case rdr_module of
	L loc (HsModule mb_mod _ imps _ _ _ _) -> let
          (Just mod) = mb_mod
	  (src_idecls, ord_idecls) = partition isSourceIdecl (map unLoc imps)
	  source_imps   = map getImpMod src_idecls
	  ordinary_imps = filter ((/= moduleName gHC_PRIM) . unLoc)
					      (map getImpMod ord_idecls)
		     -- GHC.Prim doesn't exist physically, so don't go looking for it.
	 in
	   return (source_imps, ordinary_imps, mod)


isLiskellSrcSuffix     s = s `elem` liskellish_user_src_suffixes
liskellish_user_src_suffixes = [ "lsk" ]

parseError :: SrcSpan -> Message -> IO a
parseError span err = throwOneError $ mkPlainErrMsg span err

isSourceIdecl :: ImportDecl name -> Bool
isSourceIdecl (ImportDecl _ _ s _ _ _) = s

getImpMod :: ImportDecl name -> Located ModuleName
getImpMod (ImportDecl located_mod _ _ _ _ _) = located_mod

isLiskellSrcFilename     f = isLiskellSrcSuffix     (drop 1 $ takeExtension f)
