{-# LANGUAGE CPP #-}
module LskInteractiveEval where

import LskTransformationMonad
import HsSyn
import {-# SOURCE #-} GHCSalat.GHC4Lsk as GHC4Lsk
import SrcLoc
import MonadUtils
import InteractiveEval
import HscTypes 
import LskParseTree 
import Module
import Outputable
import TcEnv
import TcExpr
import RnEnv
import RdrName
import OccName
import GHC.Exts		( unsafeCoerce# )
import GHC.Paths (libdir)

emptyModSummary = ModSummary { 
--    ms_mod       = mkModule mainPackageId interactiveMod, -- (error "pkgid") (error "modname"), -- ^ Identity of the module
    ms_hsc_src   = HsSrcFile,
    ms_location  = error "ms_location accessed",
    ms_hs_date   = error "ms_hs_date accessed",
    ms_obj_date  = error "ms_obj_date accessed",
    ms_srcimps   = error "ms_srcimps accessed",
    ms_imps      = error "ms_imps accessed",
    ms_hspp_file = error "ms_hspp_file accessed",
    ms_hspp_opts = error "ms_hspp_opts accessed",                -- ^ Cached flags from @OPTIONS@, @INCLUDE@
								       -- and @LANGUAGE@ pragmas in the modules source code
    ms_hspp_buf  = Nothing -- error "ms_hspp_buf accessed"    	-- ^ The actual preprocessed source, if we have it
  };



eval expr (imports, decls) hsc_env = do
  (PSym _ (_:newname)) <- genSym
  liftIO $ runGhc (Just libdir) $ do
      setSession hsc_env
      dflags <- getSessionDynFlags
      let prelude_mod = mkModuleName "Prelude"
#warning Recognize explicit Prelude imports
      mods <- mapM (`GHC4Lsk.findModule` Nothing) (prelude_mod:(map (unLoc . ideclName . unLoc) imports))
      liftIO $ log ("imports:" ++ (show $ length mods) ++ ", decls:" ++ (show $ length decls))
--      prel_mod <- GHC4Lsk.getPrelude
      InteractiveEval.setContext [] (mods)
--      parsed <- parseLSKModule myModSum
--      let interactiveMod = (mkModuleName "InteractiveContextModule")
      let interactiveMod = (mkModuleName $ "Adhoc" ++ newname)
      let interactiveModule = HsModule { hsmodName = Just (L noSrcSpan interactiveMod) , hsmodExports = Nothing, hsmodImports = imports, hsmodDecls = reverse decls, hsmodDeprecMessage = Nothing, hsmodHaddockModInfo = emptyHaddockModInfo, hsmodHaddockModDescr = Nothing }
      liftIO $ log "pretypecheck"
      typechecked <- typecheckModule (ParsedModule (emptyModSummary { ms_mod = mkModule mainPackageId interactiveMod, ms_hspp_opts = dflags } ) (L noSrcSpan interactiveModule))
      liftIO $ log "preload"
      loaded <- GHC4Lsk.loadModule typechecked
      liftIO $ log "postload"
      current_mod <- GHC4Lsk.findModule interactiveMod Nothing
      liftIO $ log "preset"
      InteractiveEval.setContext [current_mod] (mods)
      liftIO $ log "compiling..."
      lskTransformationMonadModule <- findModule (mkModuleName "LskTransformationMonad") Nothing
      let lskType = do 
	    lskEnvName <- lookupGlobalOccRn (mkOrig lskTransformationMonadModule
					     (mkTcOcc "LskEnvironmentTransformer"));
	    lskEnvType <- tcMetaTy lskEnvName
	    return lskEnvType
      (Just hval) <- withSession (\e -> compileHsExpr e expr lskType)
      -- We don't need to unlink that MUCH
      liftIO $ GHC4Lsk.unload hsc_env [] 
      return ((unsafeCoerce# hval) :: LskEnvironment -> IO LskEnvironment )
   where 
#ifdef VERBOSE
      log = putStrLn 
#else
      log x = return ()
#endif
