module Finder4Lsk where
import qualified  Finder as F
import HscTypes
import Module
import FastString
import DynFlags
import Data.IORef	( IORef, writeIORef, readIORef, modifyIORef )
import LazyUniqFM
import PrelNames        ( gHC_PRIM )
import System.Directory
import System.FilePath
import FiniteMap
import Util

findImportedModule :: HscEnv -> ModuleName -> Maybe FastString -> IO FindResult
findImportedModule hsc_env mod_name mb_pkg =
  case mb_pkg of
	Nothing                        -> unqual_import
	Just pkg | pkg == fsLit "this" -> home_import -- "this" is special
	         | otherwise           -> pkg_import
  where
    home_import   = findHomeModule hsc_env mod_name

    pkg_import    = F.findImportedModule hsc_env mod_name mb_pkg

    unqual_import = home_import 
			`orIfNotFound`
		      F.findImportedModule hsc_env mod_name Nothing

findHomeModule :: HscEnv -> ModuleName -> IO FindResult
findHomeModule hsc_env mod_name =
   homeSearchCache hsc_env mod_name $
   let 
     dflags = hsc_dflags hsc_env
     home_path = importPaths dflags
     hisuf = hiSuf dflags
     mod = mkModule (thisPackage dflags) mod_name

     source_exts = 
      [ ("hs",   mkHomeModLocationSearched dflags mod_name "hs")
      , ("lhs",  mkHomeModLocationSearched dflags mod_name "lhs")
      , ("lsk",  mkHomeModLocationSearched dflags mod_name "lsk")
      ]
     
     hi_exts = [ (hisuf,  	 	mkHiOnlyModLocation dflags hisuf)
	       , (addBootSuffix hisuf,	mkHiOnlyModLocation dflags hisuf)
	       ]
     
     	-- In compilation manager modes, we look for source files in the home
     	-- package because we can compile these automatically.  In one-shot
     	-- compilation mode we look for .hi and .hi-boot files only.
     exts | isOneShot (ghcMode dflags) = hi_exts
          | otherwise      	       = source_exts
   in

  -- special case for GHC.Prim; we won't find it in the filesystem.
  -- This is important only when compiling the base package (where GHC.Prim
  -- is a home module).
  if mod == gHC_PRIM 
        then return (Found (error "GHC.Prim ModLocation") mod)
        else 

   searchPathExts home_path mod exts

orIfNotFound :: IO FindResult -> IO FindResult -> IO FindResult
this `orIfNotFound` or_this = do
  res <- this
  case res of
    NotFound here _ -> do
	res2 <- or_this
	case res2 of
	   NotFound or_here pkg -> return (NotFound (here ++ or_here) pkg)
	   _other -> return res2
    _other -> return res

homeSearchCache :: HscEnv -> ModuleName -> IO FindResult -> IO FindResult
homeSearchCache hsc_env mod_name do_this = do
  m <- lookupFinderCache (hsc_FC hsc_env) mod_name
  case m of 
    Just result -> return result
    Nothing     -> do
	result <- do_this
	addToFinderCache (hsc_FC hsc_env) mod_name result
	case result of
	   Found loc mod -> addToModLocationCache (hsc_MLC hsc_env) mod loc
	   _other        -> return ()
	return result

mkHomeModLocationSearched :: DynFlags -> ModuleName -> FileExt
		          -> FilePath -> BaseName -> IO ModLocation
mkHomeModLocationSearched dflags mod suff path basename = do
   F.mkHomeModLocation2 dflags mod (path </> basename) suff

addToFinderCache :: IORef FinderCache -> ModuleName -> FindResult -> IO ()
addToFinderCache       ref key val = modifyIORef ref $ \c -> addToUFM c key val

addToModLocationCache :: IORef ModLocationCache -> Module -> ModLocation -> IO ()
addToModLocationCache  ref key val = modifyIORef ref $ \c -> addToFM c key val

lookupFinderCache :: IORef FinderCache -> ModuleName -> IO (Maybe FindResult)
lookupFinderCache ref key = do 
   c <- readIORef ref
   return $! lookupUFM c key

searchPathExts
  :: [FilePath]		-- paths to search
  -> Module		-- module name
  -> [ (
	FileExt,				-- suffix
	FilePath -> BaseName -> IO ModLocation  -- action
       )
     ] 
  -> IO FindResult

searchPathExts paths mod exts 
   = do result <- search to_search
{-
	hPutStrLn stderr (showSDoc $
		vcat [text "Search" <+> ppr mod <+> sep (map (text. fst) exts)
		    , nest 2 (vcat (map text paths))
		    , case result of
			Succeeded (loc, p) -> text "Found" <+> ppr loc
			Failed fs	   -> text "not found"])
-}	
	return result

  where
    basename = moduleNameSlashes (moduleName mod)

    to_search :: [(FilePath, IO ModLocation)]
    to_search = [ (file, fn path basename)
		| path <- paths, 
		  (ext,fn) <- exts,
		  let base | path == "." = basename
	     	           | otherwise   = path </> basename
	              file = base <.> ext
		]

    search [] = return (NotFound (map fst to_search) (Just (modulePackageId mod)))
    search ((file, mk_result) : rest) = do
      b <- doesFileExist file
      if b 
	then do { loc <- mk_result; return (Found loc mod) }
	else search rest

type FileExt = String	-- Filename extension
type BaseName = String	-- Basename of file

mkHiOnlyModLocation :: DynFlags -> Suffix -> FilePath -> String
		    -> IO ModLocation
mkHiOnlyModLocation dflags hisuf path basename
 = do let full_basename = path </> basename
      obj_fn  <- mkObjPath  dflags full_basename basename
      return ModLocation{    ml_hs_file   = Nothing,
 	        	     ml_hi_file   = full_basename <.> hisuf,
		 		-- Remove the .hi-boot suffix from
		 		-- hi_file, if it had one.  We always
		 		-- want the name of the real .hi file
		 		-- in the ml_hi_file field.
	   	             ml_obj_file  = obj_fn
                  }

mkObjPath
  :: DynFlags
  -> FilePath		-- the filename of the source file, minus the extension
  -> String		-- the module name with dots replaced by slashes
  -> IO FilePath
mkObjPath dflags basename mod_basename
  = do  let
		odir = objectDir dflags
		osuf = objectSuf dflags
	
		obj_basename | Just dir <- odir = dir </> mod_basename
			     | otherwise        = basename

        return (obj_basename <.> osuf)
