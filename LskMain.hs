{-# OPTIONS -fno-warn-incomplete-patterns -optc-DNON_POSIX_SOURCE #-}

-----------------------------------------------------------------------------
--
-- GHC Driver program
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module LskMain
where

#ifdef GHCI
--import Linker		( HValue, linkExpr )
import Desugar          ( deSugarExpr )
import InteractiveUI	( interactiveUI )
import TcRnDriver	-- ( tcRnStmt, tcRnExpr, tcRnType ) 
import PrelNames	( iNTERACTIVE )
#endif
import GHC4Lsk
import LskToHs
import LskParseTree
import LexLiskell
import ParseLiskell
import LskTransformationMonad
import GHC.Exts
import HscTypes

import DynFlags ( defaultDynFlags )
 
import Panic

import Control.Monad
import HscMain as HM (compileExpr,hscStmt)

-- The official GHC API
-- Implementations of the various modes (--show-iface, mkdependHS. etc.)
--import HscMain          ( newHscEnv )


-- Various other random stuff that we need
import HscTypes
import SrcLoc
import MonadUtils       ( liftIO )

-- Standard Haskell libraries
import Control.Monad
import Data.List
import Data.Maybe
import Module
import Parser
import Outputable
import ErrUtils
import Lexer
import StringBuffer
import FastString
import Control.Exception
import Bag



import TcRnMonad
import RnExpr
import TcSimplify
import TcExpr
import TcMType
import Inst
import TcEnv
import FamInstEnv
import TypeRep
import TcRnDriver4Lsk
---
import HscMain          hiding (compileExpr)
import HscTypes
import TcRnDriver
import Type             hiding (typeKind)
import TcType           hiding (typeKind)
import InstEnv
import Var
import Id
import IdInfo
import Name             hiding ( varName )
import NameSet
import RdrName
import VarSet
import VarEnv
import ByteCodeInstr
import Linker
import DynFlags
import Unique
import UniqSupply
import Module
import Panic
import LazyUniqFM
import Maybes
import ErrUtils
import Util
import SrcLoc
import BreakArray
import RtClosureInspect
import BasicTypes
import Outputable
import FastString
import MonadUtils

import Data.Dynamic
import Data.List (find)
import Control.Monad
import Foreign
import Foreign.C
import GHC.Exts
import Data.Array
import Exception
import Control.Concurrent
import Data.List (sortBy)
import Foreign.StablePtr
import System.IO
--

compileHsExpr		-- Compile a stmt all the way to an HValue, but don't run it
  :: GhcMonad m =>
     HscEnv
  -> LHsExpr RdrName			-- The statement
  -> m (Maybe HValue)
     -- ^ 'Nothing' <==> empty statement (or comment only), but no parse error
compileHsExpr hsc_env parsed_expr = do
--    let parsed_expr = (L noSrcSpan (HsLit (HsVar (mkFastString "Test.foo"))));
    -- Rename and typecheck it
    let icontext = hsc_IC hsc_env
    (_,Just (tc_expr,zonkedType)) <- liftIO $ tcRnExpr' hsc_env icontext parsed_expr
    let rdr_env  = ic_rn_gbl_env icontext
	type_env = mkTypeEnv (map AnId (ic_tmp_ids icontext))
    ds_expr <- ioMsgMaybe $
               deSugarExpr hsc_env iNTERACTIVE rdr_env type_env tc_expr
	-- Then desugar, code gen, and link it
    liftIO $ putStrLn (showSDoc $ ppr ds_expr)
    let src_span = srcLocSpan interactiveSrcLoc
    hval <- liftIO $ HM.compileExpr hsc_env src_span ds_expr
--    return undefined
    liftIO $ putStrLn (showSDoc $ pprType zonkedType)
    return $ Just hval

--}

parseLSKModule :: GhcMonad m => ModSummary -> m ParsedModule
parseLSKModule ms = do
   hsc_env0 <- getSession
   let hsc_env = hsc_env0 { hsc_dflags = ms_hspp_opts ms }
   stringbuffer <- liftIO $ hGetStringBuffer (ms_hspp_file ms)
   lskenv <- liftIO $ seedLskTrfEnv
   (Right (_,rdr_module)) <- liftIO $ runTM (liskell_transform_source stringbuffer noSrcLoc) (lskenv, newFreshVarStream "x") -- parseLiskell $ lexLiskell stringbuffer noSrcLoc
   return (ParsedModule ms rdr_module)

libdir = "/home/clemens/deploy/ghc-6.10.1/lib/ghc-6.10.1/"
main = 
    defaultErrorHandler defaultDynFlags  { ghcMode   = CompManager,
                  	 hscTarget = HscInterpreted,
                         ghcLink   = LinkInMemory,
        		 -- leave out hscOutName for now
                         hscOutName = panic "Main.main:hscOutName not set",
        	  	 verbosity = 0
        		} 
    $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
        dflags <- getSessionDynFlags
	let myModSum = ModSummary { 
			       ms_mod       = mkModule mainPackageId (mkModuleName "Test"), -- (error "pkgid") (error "modname"), -- ^ Identity of the module
			       ms_hsc_src   = HsSrcFile, -- error "ms_hsc_src accessed",		-- ^ The module source either plain Haskell, hs-boot or external core
			       ms_location  = error "ms_location accessed",		-- ^ Location of the various files belonging to the module
			       ms_hs_date   = error "ms_hs_date accessed",		-- ^ Timestamp of source file
			       ms_obj_date  = error "ms_obj_date accessed",	-- ^ Timestamp of object, if we have one
			       ms_srcimps   = error "ms_srcimps accessed",	-- ^ Source imports of the module
			       ms_imps      = error "ms_imps accessed",	-- ^ Non-source imports of the module
			       ms_hspp_file = "test_main.lsk", -- error "ms_hspp_file accessed",		-- ^ Filename of preprocessed source file
			       ms_hspp_opts = dflags, -- error "ms_hspp_opts accessed",                -- ^ Cached flags from @OPTIONS@, @INCLUDE@
								       -- and @LANGUAGE@ pragmas in the modules source code
			       ms_hspp_buf  = Nothing -- error "ms_hspp_buf accessed"    	-- ^ The actual preprocessed source, if we have it
			   };
--        target <- guessTarget "test_main.hs" Nothing
--        setTargets [target]
--        load LoadAllTargets
	parsed <- parseLSKModule myModSum
	typechecked <- typecheckModule parsed
	loaded <- GHC4Lsk.loadModule typechecked
--	let withlinkables = loaded { 
	prel_mod <- GHC4Lsk.findModule (GHC4Lsk.mkModuleName "Prelude") Nothing --(Just (error "hey it's him"))
	bar_mod <- GHC4Lsk.findModule (GHC4Lsk.mkModuleName "Test") (Just (error "hey it's me"))
	GHC4Lsk.setContext [] [prel_mod,bar_mod]
	lskenv <- liftIO $ seedLskTrfEnv
        (Right (_,hsExpr)) <- liftIO $ runTM (trf_expr_prim (PSym noSrcSpan "Test.foostring")) (lskenv, newFreshVarStream "x")
	hval <- withSession (\e -> compileHsExpr e hsExpr)
	let (Just thing_to_run) = unsafeCoerce# hval :: (Maybe String)
	liftIO $ putStrLn thing_to_run -- block $ forkIO $ do
	  -- return () -- Exception.try (unblock $ rethrow dflags thing_to_run) --res <-  Exception.try (unblock $ rethrow dflags thing)
--			    putMVar statusMVar (Complete res) -- empty: can't block
	return ()
