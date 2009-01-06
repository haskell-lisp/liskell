module LskNames where

#include "HsVersions.h"

import PackageConfig
import Unique	  ( Unique, Uniquable(..),
		    mkPreludeTyConUnique )
import Module	  ( Module, ModuleName, mkModule, mkModuleNameFS )
import OccName	  ( dataName, tcName, clsName, varName, mkOccNameFS,
		    mkVarOccFS )
import SrcLoc     ( noSrcLoc )
import Name	  ( mkExternalName )

liskellNames = [ lskEnvironmentTransformerTyCon ]

lSK_TRANSFORMATIONMONAD  = mkModule ghcPackageId (mkModuleNameFS FSLIT("LskTransformationMonad"))
lskEnvironmentTransformerKey         = mkPreludeTyConUnique 127 -- FIXME
lskEnvironmentTransformerTyCon       = mkExternalName lskEnvironmentTransformerKey lSK_TRANSFORMATIONMONAD 
				       (mkOccNameFS tcName FSLIT("LskEnvironmentTransformer")) 
				       Nothing noSrcLoc
