--
-- (c) The GRASP/AQUA Project, Glasgow University, 1993-2000
--

module HscMain4Lsk where
import TcRnMonad	( initIfaceCheck, TcGblEnv(..) )
import TcIface		( typecheckIface )
import ErrUtils
import HscTypes
import CorePrep		( corePrepPgm )
import TyCon		( isDataTyCon )
import CodeGen		( codeGen )
import System.IO
import Data.IORef
import Panic
import DynFlags
import CmmInfo
import CodeOutput	( codeOutput )
import TidyPgm
import MkIface
import Bag		( unitBag, emptyBag, unionBags )
import qualified HscMain as HM
import SimplCore        ( core2core )
import MonadUtils
import LoadIface	( ifaceStats, initExternalPackageState )
import Module
import CoreSyn
import StgSyn
import Id		( Id )
import CostCentre
import Cmm              ( Cmm )
import CoreToStg	( coreToStg )
import SimplStg		( stg2stg )
import CmmCPS
import TcRnDriver	( tcRnModule )
import Desugar          ( deSugar )
import Outputable
import UniqSupply	( mkSplitUniqSupply )
import CmmTx
import CmmContFlowOpt
import CmmCvt
import UniqSupply       ( initUs_ )
import CmmCPSZ
import Parser
import Lexer
import SrcLoc		( mkSrcLoc )
import SrcLoc		( Located(..) )
import HsSyn
import RdrName
import StringBuffer
import FastString
import Control.Monad
import System.Exit
import HscStats		( ppSourceStats )
import ByteCodeGen	( byteCodeGen, coreExprToBCOs )
import CodeOutput	( outputForeignStubs )
import LskFileHandler
import LskToHs
import LskTransformationMonad
import LazyUniqFM

hscNormalIface :: ModGuts -> Comp (ModIface, Bool, ModDetails, CgGuts)
hscNormalIface guts = do
  state <- gets id
  (msgs,a) <- liftIO $ HM.evalComp (HM.hscNormalIface guts) (state :: HM.CompState)
  logMsgs msgs
  return a

hscWriteIface :: (ModIface, Bool, ModDetails, a) -> Comp (ModIface, ModDetails, a)
hscWriteIface details = do
  state <- gets id
  (msgs,a) <- liftIO $ HM.evalComp (HM.hscWriteIface details) (state :: HM.CompState)
  logMsgs msgs
  return a

compHscEnv = HM.compHscEnv
compModSummary = HM.compModSummary
compOldIface = HM.compOldIface


hscCompileBatch :: Compiler (HscStatus, ModIface, ModDetails)
hscCompileBatch
   = hscCompiler norecompBatch batchMsg (genComp backend boot_backend)
   where
     backend inp  = hscSimplify inp >>= hscNormalIface >>= hscWriteIface >>= hscBatch
     boot_backend inp = hscSimpleIface inp >>= hscWriteIface >>= hscNothing

data HscStatus
    = HscNoRecomp
    | HscRecomp  Bool -- Has stub files.
                      -- This is a hack. We can't compile C files here
                      -- since it's done in DriverPipeline. For now we
                      -- just return True if we want the caller to compile
                      -- them for us.
-- Status of a compilation to byte-code.
data InteractiveStatus
    = InteractiveNoRecomp
    | InteractiveRecomp Bool     -- Same as HscStatus
                        CompiledByteCode
                        ModBreaks

norecompBatch :: NoRecomp (HscStatus, ModIface, ModDetails)
norecompBatch = norecompWorker HscNoRecomp False

norecompWorker :: a -> Bool -> NoRecomp (a, ModIface, ModDetails)
norecompWorker a _isInterp old_iface
    = do hsc_env <- gets compHscEnv
         liftIO $ do
         new_details <- {-# SCC "tcRnIface" #-}
                        initIfaceCheck hsc_env $
                        typecheckIface old_iface
         dumpIfaceStats hsc_env
         return (a, old_iface, new_details)


batchMsg :: Maybe (Int,Int) -> Bool -> Comp ()
batchMsg mb_mod_index recomp
    = do hsc_env <- gets compHscEnv
         mod_summary <- gets compModSummary
         let showMsg msg = compilationProgressMsg (hsc_dflags hsc_env) $
                           (showModuleIndex mb_mod_index ++
                            msg ++ showModMsg (hscTarget (hsc_dflags hsc_env)) recomp mod_summary)
         liftIO $ do
         if recomp
            then showMsg "Compiling "
            else if verbosity (hsc_dflags hsc_env) >= 2
                    then showMsg "Skipping  "
                    else return ()

hscSimpleIface :: TcGblEnv -> Comp (ModIface, Bool, ModDetails, TcGblEnv)
hscSimpleIface tc_result
  = do hsc_env <- gets compHscEnv
       maybe_old_iface <- gets compOldIface
       liftIO $ do
       details <- mkBootModDetailsTc hsc_env tc_result
       (new_iface, no_change)
           <- {-# SCC "MkFinalIface" #-}
              mkIfaceTc hsc_env (fmap mi_iface_hash maybe_old_iface) details tc_result
       -- And the answer is ...
       dumpIfaceStats hsc_env
       return (new_iface, no_change, details, tc_result)

hscNothing :: (ModIface, ModDetails, a) -> Comp (Maybe (HscStatus, ModIface, ModDetails))
hscNothing (iface, details, _)
    = return (Just (HscRecomp False, iface, details))


hscSimplify :: ModGuts -> Comp ModGuts
hscSimplify ds_result
  = do hsc_env <- gets compHscEnv
       liftIO $ do
           -------------------
           -- SIMPLIFY
           -------------------
       simpl_result <- {-# SCC "Core2Core" #-}
                       core2core hsc_env ds_result
       return simpl_result

--------------------------------------------------------------
-- BackEnd combinators
--------------------------------------------------------------

-- Generate code and return both the new ModIface and the ModDetails.
hscBatch :: (ModIface, ModDetails, CgGuts) -> Comp (Maybe (HscStatus, ModIface, ModDetails))
hscBatch (iface, details, cgguts)
    = do hasStub <- hscCompile cgguts
         return (Just (HscRecomp hasStub, iface, details))

-- Compile to hard-code.
hscCompile :: CgGuts -> Comp Bool
hscCompile cgguts
    = do hsc_env <- gets compHscEnv
         mod_summary <- gets compModSummary
         liftIO $ do
         let CgGuts{ -- This is the last use of the ModGuts in a compilation.
                     -- From now on, we just use the bits we need.
                     cg_module   = this_mod,
                     cg_binds    = core_binds,
                     cg_tycons   = tycons,
                     cg_dir_imps = dir_imps,
                     cg_foreign  = foreign_stubs,
                     cg_dep_pkgs = dependencies,
		     cg_hpc_info = hpc_info } = cgguts
             dflags = hsc_dflags hsc_env
             location = ms_location mod_summary
             data_tycons = filter isDataTyCon tycons
             -- cg_tycons includes newtypes, for the benefit of External Core,
             -- but we don't generate any code for newtypes

         -------------------
         -- PREPARE FOR CODE GENERATION
         -- Do saturation and convert to A-normal form
         prepd_binds <- {-# SCC "CorePrep" #-}
                        corePrepPgm dflags core_binds data_tycons ;
         -----------------  Convert to STG ------------------
         (stg_binds, cost_centre_info)
             <- {-# SCC "CoreToStg" #-}
                myCoreToStg dflags this_mod prepd_binds	
         ------------------  Code generation ------------------
         cmms <- {-# SCC "CodeGen" #-}
                      codeGen dflags this_mod data_tycons
                              dir_imps cost_centre_info
                              stg_binds hpc_info
         --- Optionally run experimental Cmm transformations ---
         cmms <- optionallyConvertAndOrCPS hsc_env cmms
                 -- unless certain dflags are on, the identity function
         ------------------  Code output -----------------------
         rawcmms <- cmmToRawCmm cmms
         (_stub_h_exists, stub_c_exists)
             <- codeOutput dflags this_mod location foreign_stubs 
                dependencies rawcmms
         return stub_c_exists


hscCompiler
        :: NoRecomp result                       -- No recomp necessary
        -> (Maybe (Int,Int) -> Bool -> Comp ())  -- Message callback
        -> Comp (Maybe result)
        -> Compiler result
hscCompiler norecomp messenger recomp hsc_env mod_summary 
            source_unchanged mbOldIface mbModIndex
 = do
    --    liftIO $ putStrLn ("hscCompiler home package table length " ++ (show (length $ ufmToList $ (hsc_HPT hsc_env))))
    ioMsgMaybe $ 
      flip evalComp (HM.CompState hsc_env mod_summary mbOldIface) $
      do (recomp_reqd, mbCheckedIface)
             <- {-# SCC "checkOldIface" #-}
                liftIO $ checkOldIface hsc_env mod_summary
                              source_unchanged mbOldIface
	 -- save the interface that comes back from checkOldIface.
	 -- In one-shot mode we don't have the old iface until this
	 -- point, when checkOldIface reads it from the disk.
	 modify (\s -> s{ HM.compOldIface = mbCheckedIface })
         case mbCheckedIface of 
           Just iface | not recomp_reqd
               -> do messenger mbModIndex False
                     result <- norecomp iface
                     return (Just result)
           _otherwise
               -> do messenger mbModIndex True
                     recomp

-- the usual way to build the Comp (Maybe result) to pass to hscCompiler
genComp :: (ModGuts  -> Comp (Maybe a))
        -> (TcGblEnv -> Comp (Maybe a))
        -> Comp (Maybe a)
genComp backend boot_backend = do
    mod_summary <- gets compModSummary
    case ms_hsc_src mod_summary of
       ExtCoreFile -> do
          panic "GHC does not currently support reading External Core files"
       _not_core -> do
          mb_tc <- hscFileFrontEnd
          case mb_tc of
            Nothing -> return Nothing
            Just tc_result -> 
              case ms_hsc_src mod_summary of
                HsBootFile -> boot_backend tc_result
                _other     -> do
                  mb_guts <- hscDesugar tc_result
                  case mb_guts of
                    Nothing -> return Nothing
                    Just guts -> backend guts


-- I want Control.Monad.State! --Lemmih 03/07/2006
newtype Comp a = Comp {runComp :: HM.CompState -> IORef Messages -> IO (a, HM.CompState)}

instance Monad Comp where
    g >>= fn = Comp $ \s r -> runComp g s r >>= \(a,s') -> runComp (fn a) s' r
    return a = Comp $ \s _ -> return (a,s)
    fail = error

evalComp :: Comp a -> HM.CompState -> IO (Messages, a)
evalComp comp st = do r <- newIORef emptyMessages
                      (val,_st') <- runComp comp st r
                      msgs <- readIORef r
                      return (msgs, val)

logMsgs :: Messages -> Comp ()
logMsgs (warns', errs') = Comp $ \s r -> do
                           (warns, errs) <- readIORef r
                           writeIORef r $! ( warns' `unionBags` warns
                                           , errs' `unionBags` errs )
                           return ((), s)

get :: Comp HM.CompState
get = Comp $ \s _ -> return (s,s)

modify :: (HM.CompState -> HM.CompState) -> Comp ()
modify f = Comp $ \s _ -> return ((), f s)

gets :: (HM.CompState -> a) -> Comp a
gets getter = do st <- get
                 return (getter st)

instance MonadIO Comp where
  liftIO ioA = Comp $ \s _ -> do a <- ioA; return (a,s)

type NoRecomp result = ModIface -> Comp result

-- FIXME: The old interface and module index are only using in 'batch' and
--        'interactive' mode. They should be removed from 'oneshot' mode.
type Compiler result =  GhcMonad m =>
                        HscEnv
                     -> ModSummary
                     -> Bool                -- True <=> source unchanged
                     -> Maybe ModIface      -- Old interface, if available
                     -> Maybe (Int,Int)     -- Just (i,n) <=> module i of n (for msgs)
                     -> m result

dumpIfaceStats :: HscEnv -> IO ()
dumpIfaceStats hsc_env
  = do	{ eps <- readIORef (hsc_EPS hsc_env)
	; dumpIfSet (dump_if_trace || dump_rn_stats)
	      	    "Interface statistics"
	      	    (ifaceStats eps) }
  where
    dflags = hsc_dflags hsc_env
    dump_rn_stats = dopt Opt_D_dump_rn_stats dflags
    dump_if_trace = dopt Opt_D_dump_if_trace dflags

showModuleIndex :: Maybe (Int, Int) -> String
showModuleIndex Nothing = ""
showModuleIndex (Just (i,n)) = "[" ++ padded ++ " of " ++ n_str ++ "] "
    where
        n_str = show n
        i_str = show i
        padded = replicate (length n_str - length i_str) ' ' ++ i_str

myCoreToStg :: DynFlags -> Module -> [CoreBind]
            -> IO ( [(StgBinding,[(Id,[Id])])]  -- output program
	          , CollectedCCs) -- cost centre info (declared and used)

myCoreToStg dflags this_mod prepd_binds
 = do 
      stg_binds <- {-# SCC "Core2Stg" #-}
	     coreToStg (thisPackage dflags) prepd_binds

      (stg_binds2, cost_centre_info) <- {-# SCC "Stg2Stg" #-}
	     stg2stg dflags this_mod stg_binds

      return (stg_binds2, cost_centre_info)

optionallyConvertAndOrCPS :: HscEnv -> [Cmm] -> IO [Cmm]
optionallyConvertAndOrCPS hsc_env cmms =
    do let dflags = hsc_dflags hsc_env
        --------  Optionally convert to and from zipper ------
       cmms <- if dopt Opt_ConvertToZipCfgAndBack dflags
               then mapM (testCmmConversion hsc_env) cmms
               else return cmms
         ---------  Optionally convert to CPS (MDA) -----------
       cmms <- if not (dopt Opt_ConvertToZipCfgAndBack dflags) &&
                  dopt Opt_RunCPSZ dflags
               then cmmCPS dflags cmms
               else return cmms
       return cmms

hscFileFrontEnd :: Comp (Maybe TcGblEnv)
hscFileFrontEnd =
    do hsc_env <- gets compHscEnv
       mod_summary <- gets compModSummary

             -------------------
             -- PARSE
             -------------------
       let dflags = hsc_dflags hsc_env
           hspp_file = ms_hspp_file mod_summary
           hspp_buf  = ms_hspp_buf  mod_summary
	   
       maybe_parsed <-
           if isLiskellSrcFilename hspp_file then
               liftIO $ myParseLiskellModule dflags hspp_file hspp_buf (newFreshVarStream (moduleNameString $ ms_mod_name mod_summary)) hsc_env
           else
               liftIO $ myParseModule dflags hspp_file hspp_buf
       case maybe_parsed of
         Left err
             -> do logMsgs (emptyBag, unitBag err)
                   return Nothing
         Right rdr_module
             -------------------
             -- RENAME and TYPECHECK
             -------------------
             -> do (tc_msgs, maybe_tc_result) 
                       <- {-# SCC "Typecheck-Rename" #-}
                          liftIO $ tcRnModule hsc_env (ms_hsc_src mod_summary)
                                              False rdr_module
                   logMsgs tc_msgs
                   return maybe_tc_result

--------------------------------------------------------------
-- Desugaring
--------------------------------------------------------------

hscDesugar :: TcGblEnv -> Comp (Maybe ModGuts)
hscDesugar tc_result
  = do mod_summary <- gets compModSummary
       hsc_env <- gets compHscEnv

          -------------------
          -- DESUGAR
          -------------------
       (msgs, ds_result)
           <- {-# SCC "DeSugar" #-}
              liftIO $ deSugar hsc_env (ms_location mod_summary) tc_result
       logMsgs msgs
       return ds_result

testCmmConversion :: HscEnv -> Cmm -> IO Cmm
testCmmConversion hsc_env cmm =
    do let dflags = hsc_dflags hsc_env
       showPass dflags "CmmToCmm"
       dumpIfSet_dyn dflags Opt_D_dump_cvt_cmm "C-- pre-conversion" (ppr cmm)
       --continuationC <- cmmCPS dflags abstractC >>= cmmToRawCmm
       us <- mkSplitUniqSupply 'C'
       let cfopts = runTx $ runCmmOpts cmmCfgOptsZ
       let cvtm = do g <- cmmToZgraph cmm
                     return $ cfopts g
       let zgraph = initUs_ us cvtm
       cps_zgraph <- protoCmmCPSZ hsc_env zgraph
       let chosen_graph = if dopt Opt_RunCPSZ dflags then cps_zgraph else zgraph
       dumpIfSet_dyn dflags Opt_D_dump_cmmz "C-- Zipper Graph" (ppr chosen_graph)
       showPass dflags "Convert from Z back to Cmm"
       let cvt = cmmOfZgraph $ cfopts $ chosen_graph
       dumpIfSet_dyn dflags Opt_D_dump_cvt_cmm "C-- post-conversion" (ppr cvt)
       return cvt
       -- return cmm -- don't use the conversion


myParseModule :: DynFlags -> FilePath -> Maybe StringBuffer
              -> IO (Either ErrMsg (Located (HsModule RdrName)))
myParseModule dflags src_filename maybe_src_buf
 =    --------------------------  Parser  ----------------
      showPass dflags "Parser" >>
      {-# SCC "Parser" #-} do

	-- sometimes we already have the buffer in memory, perhaps
	-- because we needed to parse the imports out of it, or get the 
	-- module name.
      buf <- case maybe_src_buf of
		Just b  -> return b
		Nothing -> hGetStringBuffer src_filename

      let loc  = mkSrcLoc (mkFastString src_filename) 1 0

      case unP parseModule (mkPState buf loc dflags) of {

	PFailed span err -> return (Left (mkPlainErrMsg span err));

	POk pst rdr_module -> do {

      let {ms = getMessages pst};
      printErrorsAndWarnings dflags ms; -- XXX
      when (errorsFound dflags ms) $ exitWith (ExitFailure 1);
      
      dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr rdr_module) ;
      
      dumpIfSet_dyn dflags Opt_D_source_stats "Source Statistics"
			   (ppSourceStats False rdr_module) ;
      
      return (Right rdr_module)
	-- ToDo: free the string buffer later.
      }}

myParseLiskellModule dflags src_filename maybe_src_buf fresh_var_stream hsc_env
 =    --------------------------  Parser  ----------------
      showPass dflags "Parser" >>
      {-# SCC "Parser" #-} do

       -- sometimes we already have the buffer in memory, perhaps
       -- because we needed to parse the imports out of it, or get the 
       -- module name.
      buf <- case maybe_src_buf of
               Just b  -> return b
               Nothing -> hGetStringBuffer src_filename

      let loc  = mkSrcLoc (mkFastString src_filename) 1 0

      env <- seedLskTrfEnv

      module_t <- runTM (liskell_transform_source buf loc) (TransformationState env hsc_env fresh_var_stream ([], []))

      case module_t of { 

       (Left (TrErr span err)) -> return (Left (mkPlainErrMsg span err));

       Right (new_fresh_vars,rdr_module) ->
           do {
             dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" (ppr rdr_module);
             dumpIfSet_dyn dflags Opt_D_source_stats "Source Statistics" (ppSourceStats False rdr_module);

             return (Right rdr_module)
           -- ToDo: free the string buffer later.
      }}

-- Type-check Haskell and .hs-boot only (no external core)
hscCompileNothing :: Compiler (HscStatus, ModIface, ModDetails)
hscCompileNothing
   = hscCompiler norecompBatch batchMsg comp
   where
     backend tc = hscSimpleIface tc >>= hscIgnoreIface >>= hscNothing

     comp = do   -- genComp doesn't fit here, because we want to omit
                 -- desugaring and for the backend to take a TcGblEnv
        mod_summary <- gets compModSummary
        case ms_hsc_src mod_summary of
           ExtCoreFile -> panic "hscCompileNothing: cannot do external core"
           _other -> do
                mb_tc <- hscFileFrontEnd
                case mb_tc of
                  Nothing -> return Nothing
                  Just tc_result -> backend tc_result

hscIgnoreIface :: (ModIface, Bool, ModDetails, a) -> Comp (ModIface, ModDetails, a)
hscIgnoreIface (iface, _no_change, details, a)
    = return (iface, details, a)

hscCompileInteractive :: Compiler (InteractiveStatus, ModIface, ModDetails)
hscCompileInteractive
   = hscCompiler norecompInteractive batchMsg (genComp backend boot_backend)
   where
     backend inp = hscSimplify inp >>= hscNormalIface >>= hscIgnoreIface >>= hscInteractive
     boot_backend _ = panic "hscCompileInteractive: HsBootFile"

norecompInteractive :: NoRecomp (InteractiveStatus, ModIface, ModDetails)
norecompInteractive = norecompWorker InteractiveNoRecomp True


hscInteractive :: (ModIface, ModDetails, CgGuts)
               -> Comp (Maybe (InteractiveStatus, ModIface, ModDetails))
#ifdef GHCI
hscInteractive (iface, details, cgguts)
    = do hsc_env <- gets compHscEnv
         mod_summary <- gets compModSummary
         liftIO $ do
         let CgGuts{ -- This is the last use of the ModGuts in a compilation.
                     -- From now on, we just use the bits we need.
                     cg_module   = this_mod,
                     cg_binds    = core_binds,
                     cg_tycons   = tycons,
                     cg_foreign  = foreign_stubs,
                     cg_modBreaks = mod_breaks } = cgguts
             dflags = hsc_dflags hsc_env
             location = ms_location mod_summary
             data_tycons = filter isDataTyCon tycons
             -- cg_tycons includes newtypes, for the benefit of External Core,
             -- but we don't generate any code for newtypes

         -------------------
         -- PREPARE FOR CODE GENERATION
         -- Do saturation and convert to A-normal form
         prepd_binds <- {-# SCC "CorePrep" #-}
                        corePrepPgm dflags core_binds data_tycons ;
         -----------------  Generate byte code ------------------
         comp_bc <- byteCodeGen dflags prepd_binds data_tycons mod_breaks
         ------------------ Create f-x-dynamic C-side stuff ---
         (_istub_h_exists, istub_c_exists) 
             <- outputForeignStubs dflags this_mod location foreign_stubs
         return (Just (InteractiveRecomp istub_c_exists comp_bc mod_breaks, iface, details))
#else
hscInteractive _ = panic "GHC not compiled with interpreter"
#endif
