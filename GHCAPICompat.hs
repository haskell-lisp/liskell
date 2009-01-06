--								-*-haskell-*-
-- ---------------------------------------------------------------------------
-- Liskell
--
-- Author(s):
-- ---------------------------------------------------------------------------

module GHCAPICompat where

import HsSyn
import RdrHsSyn
cHsModule a b c d e = HsModule a b c d e emptyHaddockModInfo Nothing
cmkClassDecl a b c d = mkClassDecl a b c d [] []
ccvBindsAndSigs x = let (binds, sigs, _, _ ) = cvBindsAndSigs x
                    in (binds, sigs)

cConDecl a b c d e f = ConDecl a b c d e f Nothing
cTySynonym a b c = TySynonym a b Nothing c
cmkTyData a (b, c, d) e f g = mkTyData a (b, c, d, Nothing) e f g
cInstDecl a b c = InstDecl a b c
