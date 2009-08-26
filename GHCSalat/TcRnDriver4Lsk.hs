{-# OPTIONS -fno-warn-incomplete-patterns -optc-DNON_POSIX_SOURCE #-}

-----------------------------------------------------------------------------
--
-- GHC Driver program
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module GHCSalat.TcRnDriver4Lsk
where

#ifdef GHCI
--import Linker		( HValue, linkExpr )
import Desugar          ( deSugarExpr )
import GHCSalat.InteractiveUI	( interactiveUI )
import TcRnDriver	-- ( tcRnStmt, tcRnExpr, tcRnType ) 
import PrelNames	( iNTERACTIVE )
#endif
import LskToHs
import LskParseTree
import LskTransformationMonad
import GHC.Exts
import HscTypes
import HsSyn
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

setInteractiveContext' :: HscEnv -> InteractiveContext -> TcRn a -> TcRn a
setInteractiveContext' hsc_env icxt thing_inside 
  = let -- Initialise the tcg_inst_env with instances from all home modules.  
        -- This mimics the more selective call to hptInstances in tcRnModule.
	(home_insts, home_fam_insts) = hptInstances hsc_env (\mod -> True)
    in
    updGblEnv (\env -> env { 
	tcg_rdr_env      = ic_rn_gbl_env icxt,
	tcg_inst_env     = extendInstEnvList    (tcg_inst_env env) home_insts,
	tcg_fam_inst_env = extendFamInstEnvList (tcg_fam_inst_env env) 
                                                home_fam_insts 
      }) $

    tcExtendGhciEnv (ic_tmp_ids icxt) $
        -- tcExtendGhciEnv does lots: 
        --   - it extends the local type env (tcl_env) with the given Ids,
        --   - it extends the local rdr env (tcl_rdr) with the Names from 
        --     the given Ids
        --   - it adds the free tyvars of the Ids to the tcl_tyvars
        --     set.
        --
        -- later ids in ic_tmp_ids must shadow earlier ones with the same
        -- OccName, and tcExtendIdEnv implements this behaviour.

    do	{ traceTc (text "setIC" <+> ppr (ic_tmp_ids icxt))
 	; thing_inside }


tcRnExpr' :: HscEnv
	 -> InteractiveContext
	 -> LHsExpr RdrName
	 -> IO (Messages, Maybe (LHsExpr TcId, Type))
tcRnExpr' hsc_env ictxt rdr_expr
  = initTcPrintErrors hsc_env (error "Juhu") $ 
    setInteractiveContext' hsc_env ictxt $ do {

    (rn_expr, fvs) <- rnLExpr rdr_expr ;
    failIfErrsM ;

	-- Now typecheck the expression; 
	-- it might have a rank-2 type (e.g. :t runST)
    ((tc_expr, res_ty), lie)	   <- getLIE (tcInferRho rn_expr) ;
    ((qtvs, dict_insts, _), lie_top) <- getLIE (tcSimplifyInfer smpl_doc (tyVarsOfType res_ty) lie)  ;
    tcSimplifyInteractive lie_top ;

    let { all_expr_ty = mkForAllTys qtvs $
    		        mkFunTys (map (idType . instToId) dict_insts)	$
    		        res_ty } ;
    zonked <- zonkTcType all_expr_ty;
    return (tc_expr, zonked)
    }
  where
    smpl_doc = ptext (sLit "main expression")

