module GHCSalat.GHC4Lsk (ParsedMod,loadModule,typecheckModule,runGhc,setSessionDynFlags,getSessionDynFlags,findModule,ParsedModule(..),compileHsExpr,setTargets,LoadHowMuch(..),load,unload) where
import GHCSalat.GhciMonad
import HscTypes
import Module
import FastString
import DynFlags
import SrcLoc
import HsSyn
import RdrName
import TcRnTypes        hiding (LIE)
import Name
import Var
import ByteCodeLink
import BasicTypes 
import TypeRep

findModule :: GhcMonad m => ModuleName -> Maybe FastString -> m Module

getSessionDynFlags :: GhcMonad m => m DynFlags

setSessionDynFlags :: GhcMonad m => DynFlags -> m [PackageId]

runGhc :: Maybe FilePath  -- ^ See argument to 'initGhcMonad'.
       -> Ghc a           -- ^ The action to perform.
       -> IO a

loadModule :: (TypecheckedMod mod, GhcMonad m) => mod -> m mod

typecheckModule :: GhcMonad m => ParsedModule -> m TypecheckedModule

class ParsedMod m => TypecheckedMod m where
  renamedSource     :: m -> Maybe RenamedSource
  typecheckedSource :: m -> TypecheckedSource
  moduleInfo        :: m -> ModuleInfo
  tm_internals      :: m -> (TcGblEnv, ModDetails)
	-- ToDo: improvements that could be made here:
	--  if the module succeeded renaming but not typechecking,
	--  we can still get back the GlobalRdrEnv and exports, so
	--  perhaps the ModuleInfo should be split up into separate
	--  fields.

class ParsedMod m where

data TypecheckedModule 
type RenamedSource     = (HsGroup Name, [LImportDecl Name], Maybe [LIE Name],
                          Maybe (HsDoc Name), HaddockModInfo Name)
type TypecheckedSource = LHsBinds Id

data ParsedModule =
  ParsedModule { pm_mod_summary   :: ModSummary
               , pm_parsed_source :: ParsedSource }
type ParsedSource      = Located (HsModule RdrName)


instance TypecheckedMod TypecheckedModule where

data ModuleInfo

compileHsExpr		-- Compile a stmt all the way to an HValue, but don't run it
  :: GhcMonad m =>
     HscEnv
  -> LHsExpr RdrName			-- The statement
  -> TcM Type
  -> m (Maybe HValue)

setTargets :: GhcMonad m => [Target] -> m ()

data LoadHowMuch
   = LoadAllTargets
   | LoadUpTo ModuleName
   | LoadDependenciesOf ModuleName

load :: GhcMonad m => LoadHowMuch -> m SuccessFlag

unload :: HscEnv -> [Linkable] -> IO ()
