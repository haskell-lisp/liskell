--								-*-haskell-*-
-- ---------------------------------------------------------------------------
-- Liskell
-- ParseTree to Haskell ParseTree conversion
--
-- Author(s): Clemens Fruhwirth <clemens@endorphin.org>
-- ---------------------------------------------------------------------------

module LskToHs where

import ParseLiskell
import LexLiskell
import LskParseTree
import LskTransformationMonad
import GHCAPICompat
import HsSyn
import SrcLoc
import Module
import Outputable
import RdrName
import FastString
import OccName
import RdrHsSyn
import OrdList
import qualified BasicTypes
import StringBuffer
import Bag
import TysWiredIn	( unitTyCon, unitDataCon, tupleTyCon, tupleCon, nilDataCon,
			  listTyCon_RDR, parrTyCon_RDR, consDataCon_RDR )
-- verify that all these types are used ^^ FIXMELSk
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.Char
import qualified Numeric
import PackageConfig
import Util
import LskInteractiveEval
import ReadRationalS

type SimpleType = ((Located RdrName), [LHsTyVarBndr RdrName])

orElse' :: Maybe a -> Maybe a -> Maybe a
orElse' a b =
    case a of
      (Just _) -> a
      Nothing -> b

-------------------------------------------------------------------------------------
--- Symbol classifiers and convertes
-------------------------------------------------------------------------------------

isUpSymId string
    | null string = error "internal error - isUpSymId called for null id"
    | otherwise	  = elem (head string) (['A'..'Z'] ++ [':'])
isLowSymId = not . isUpSymId

isUpId str =
    let (orig, qual, sym) = parseOrig str
    in isUpSymId sym
isLowId = not . isUpId

mkRdrName2 otherspace upspace (PSym _ str) =
    let (orig, qual, sym) = parseOrig str
	space | isUpSymId sym = upspace
	      | otherwise     = otherspace
    in case (orig, qual, sym, space == dataName, space == tcClsName) of
	 -- FIXMELSK do we really need the built-in type handling
	 -- below? It doesn't look like as if the sky would came
	 -- crushing down on GHC if we omit that.
	 ("", "", ":", True, False) -> consDataCon_RDR
--	 ("", "", "[]", False, True) -> listTyCon_RDR
--	 ("", "", ",", True, False) -> tupleCon <boxity> <size>
--	 ("", "", ",", False, True) -> tupleTyCon <boxity> <size>
	 ("", "", sym, _, _) -> mkUnqual space (mkFastString sym)
	 ("", qual, sym, _, _) -> mkQual space ((mkFastString qual), (mkFastString sym))
	 (orig, qual, sym, _, _) -> mkOrig (mkModule (stringToPackageId orig) (mkModuleName qual)) (mkOccName space sym)

mkUnqualS space string = mkUnqual space (mkFastString string)

mkLRdrName2 otherspace upspace sym@(PSym loc _) =
    (L loc (mkRdrName2 otherspace upspace sym))

-------------------------------------------------------------------------------------
--- Module Transformations (Name, Imports, Exports)
-------------------------------------------------------------------------------------

trf_modulename (PSym _ m)
    | isUpSymId m = return (mkModuleName m)
    | otherwise = throwError (text "Unknown module name syntax")

--- Transform IE names
trf_iename :: ParseTree -> TransformationMonad (LIE RdrName)
trf_iename (PSym loc str)
    | isUpSymId str = return (L loc (IEThingAbs (mkUnqualS tcClsName str)))
    | otherwise     = return (L loc (IEVar (mkUnqualS varName str)))

trf_iename (PList loc (s@(PSym _ sym):rest) ("",""))
    | sym == "module" = case rest of
                          modname:[] -> do
                                    modname_t <- trf_modulename modname
				    return (L loc (IEModuleContents modname_t))
			  otherwise -> throwError (text "Wrong arity for module in exports")
    | isUpSymId sym =
        case rest of
          (PSym loc "_"):rest ->
              if (null rest) then
                  return (L loc (IEThingAll (mkUnqualS tcClsName sym)))
              else 
                  throwError (text "Wrong wildcard form for iename")
          otherwise -> return (L loc (IEThingWith (mkUnqualS tcClsName sym)
                                        (map (mkRdrName2 varName dataName) rest)))

--- Transform IE exports
trf_exports :: ParseTree -> TransformationMonad (Maybe [LIE RdrName])
trf_exports (PSym _ sym)
    | sym == "_" = return Nothing
    | otherwise = throwError (text "Unexpected symbol in exports. Only \"_\" is valid.")

trf_exports (PList _ exports ("","")) = 
    do
      exports_t <- mapM trf_iename exports
      return $ Just exports_t

trf_exports p = throwError (text ("Unknown export" ++ show p))

--- Transform imports
flaglist :: ParseTree -> Maybe [ParseTree]
flaglist (PList _ ((PSym _ s):flaglist) ("",""))
  | s == "flags" = Just flaglist
  | otherwise = Nothing
flaglist _ = Nothing

parse_flags [] = return (False, Nothing, False)
parse_flags ((PSym _ "hiding"):xs) = do
  (a, b, c) <- parse_flags xs
  return (a, b, True)

parse_flags ((PList _ [(PSym _ "as"), modulename] ("","")):xs) = do
  (a, b, c) <- parse_flags xs
  module_name_t <- trf_modulename modulename
  return (a, (Just module_name_t), c)

parse_flags ((PSym _ "qualified"):xs) = do
  (a, b, c) <- parse_flags xs
  return (True, b, c)

trf_import ms@(PSym loc _) = do
  modulename_t <- trf_modulename ms 
  return $ Just (L loc (ImportDecl { 
			  ideclName = (L loc modulename_t),
			  ideclPkgQual = Nothing,
			  ideclSource = False,
			  ideclQualified = False,
			  ideclAs     = Nothing,
			  ideclHiding = Nothing 
			}))

trf_import (PList loc (modulename:ienames) ("","")) =
    do 
      modulename_t <- trf_modulename modulename
      case ienames of
        maybeflags:restienames -> 
            case (flaglist maybeflags) of
              Just flags -> do 
                     imports <- mapM trf_iename restienames
                     (qual, asModuleName, hiding) <- parse_flags flags
                     return $ Just (L loc (ImportDecl {
					     ideclName = (L (ploc modulename) modulename_t),
					     ideclPkgQual = Nothing,
					     ideclSource = False,
					     ideclQualified = qual,
					     ideclAs = asModuleName,
					     ideclHiding = (Just (hiding, imports)) }))
              Nothing -> do
                     imports <- mapM trf_iename ienames
                     return $ Just (L loc (ImportDecl { 
					     ideclName = (L (ploc modulename) modulename_t),
					     ideclPkgQual = Nothing,
					     ideclSource = False,
					     ideclQualified = False,
					     ideclAs     = Nothing,
					     ideclHiding = (Just (False, imports))}))
        otherwise -> return Nothing

-------------------------------------------------------------------------------------
--- Binder Transformations 
-------------------------------------------------------------------------------------

-- The ET/TE Funbinder syntax consists of an EBinder augmented with a signature for that binder.
-- The full binder in the form (V T E) can be truncated to an eBinder of the form (V E) using the full binder
-- (V _ E).
--- The full binder (V T E) can be truncated to a signature declaration (V T) omitting the expression as in 
-- (V T _).
-- The trf_fullbind converts the given ParseTree to a list of declaration (with one or two elements).

errorSpan = error "SrcSpan of fake symbol inspected!"

trf_TEFunbinder :: ParseTree -> TransformationMonad [LHsDecl RdrName]
trf_TEFunbinder (PList loc [binder, typesig] ("","")) = trf_TEFunbinder (PList loc [binder, typesig, (PSym errorSpan "_")] noPP)
trf_TEFunbinder (PList loc [binding, typesig, expr] ("","")) = do
    binding_t <- trf_EBinder binding (case expr of
                                     (PSym _ "_") -> (PSym errorSpan "undefined") -- FIX?
                                     otherwise -> expr)
    typesig_t <- case typesig of 
                 (PSym _ "_") -> return []
                 otherwise -> do
                              type_t <- trf_sig_type typesig
                              case binding_t of 
                                (FunBind { fun_id = id }) -> return [(L loc (SigD (TypeSig id type_t)))]
                                otherwise -> throwError (text ("Nothing but funbinders are allowed for ET/TE-funbinder"))
    return (case expr of
                 (PSym _ "_") -> typesig_t
                 otherwise -> concat [typesig_t, [(L loc (ValD binding_t))]])
trf_TEFunbinder p = throwErrorAt (ploc p) (text ("Unknown TEFunbinder" ++ show p))

trf_ETFunbinder (PList loc [binding, expr] ("","")) = trf_TEFunbinder (PList loc [binding, (PSym errorSpan "_"), expr] noPP)
trf_ETFunbinder (PList loc [binding, expr, typesig] ("","")) = trf_TEFunbinder (PList loc [binding, typesig, expr] noPP)
trf_ETFunbinder p = throwErrorAt (ploc p) (text ("Unknown TEFunbinder" ++ show p))

---
--- Binding Syntax for HsBind - this is aka eBinder Syntax, as in Expression Binder
---
--- Atom         => FunBinder
--- (LowId _ ..) => FunBinder
--- (UpId _ ..)  => PatBind
---
--- 

trf_EBinder pat expr = do
    expr_t <- trf_expr expr

    -- Try to parse this as a pattern
    -- If it succeeds, create a pattern binder
    -- If it fails, create a funbinders
    catchError
      (do
	pat <- trf_pattern pat
	return (case (unLoc pat) of
		  (VarPat id) -> mkFunBind (L (getLoc pat) id) [(mkSimpleMatch [] expr_t)]
		  _           -> PatBind { pat_lhs = pat,
					   pat_rhs = GRHSs [(noLoc (GRHS [] expr_t))] EmptyLocalBinds,
					   pat_rhs_ty = placeHolderType,
					   bind_fvs = placeHolderNames }))
      (const (let (PList floc (id@(PSym vloc sym):args) ("","")) = pat
              in do
		patterns_t <- mapM trf_pattern args
		return (mkFunBind (L vloc (mkRdrName2 varName (error "Touched Upspace for vars") id)) [(mkSimpleMatch patterns_t expr_t)])))

-- Transform a list of bindings into a OrdList (LHsDecl ..) of ValD
-- declarations. No other declaration is introduced by let/define styled
-- binding syntax

trf_letbindings [] = return nilOL
trf_letbindings ((PList _ [head, expr] ("","")):xs) = do
  rest_t <- trf_letbindings xs
  first_t <- trf_EBinder head expr
  return ((unitOL (noLoc (ValD first_t))) `appOL` rest_t)
trf_letbindings (pt:_) = throwErrorAt (ploc pt) (vcat [text "Unknown let bindings", text (show pt)])

--- Match Transformation
trf_match (PList loc [pat_e, expr_e] ("","")) =
    do 
      pat_t <- trf_pattern pat_e
      expr_t <- trf_expr expr_e
      return (mkSimpleMatch [pat_t] expr_t)
trf_match pt = throwErrorAt (ploc pt) (vcat [text "Unknown match form", text (show pt)])

--- FIXME Ugly function name
mapMaybeFuns (first:rest) = do 
  maybetransformed <- first
  case maybetransformed of
    Just a -> return a
    Nothing -> mapMaybeFuns rest
mapMayBeFuns [] = throwError (text "Dropped to bottom of transformer table")

mapTransformer envGet pt = do
  env <- askEnv
  mapMaybeFuns (map (\fun -> fun pt) (envGet env))

-------------------------------------------------------------------------------------
--- Expression Transformations to HsExpr  
-------------------------------------------------------------------------------------

trf_expr :: ParseTree -> TransformationMonad (LHsExpr RdrName)
trf_expr pt = do
  env <- askEnv
  (envExprTable env) pt trf_expr trf_expr_prim

trf_expr_prim :: ParseTree -> TransformationMonad (LHsExpr RdrName)
trf_expr_prim sym@(PSym loc symname)
  | Just int <- convertNumber symname =
      return (L loc (HsOverLit (mkHsIntegral int placeHolderType)))
  | Just rational <- convertRational symname =
      return (L loc (HsOverLit (mkHsFractional rational placeHolderType)))
  | Just char <- convertChar symname =
      return (L loc (HsLit (HsChar char)))
  | Just string <- convertString symname =

      return (L loc (HsLit (HsString (mkFastString string))))
  | otherwise =
      return (L loc (HsVar (mkRdrName2 varName dataName sym)))

trf_expr_prim (PList loc full@((PSym lloc sym):rest) ("",""))
    | sym == "if" =
        case rest of
          cond_e:then_e:else_e:[] ->
              do 
                cond_t <- trf_expr cond_e
                then_t <- trf_expr then_e
                expr_t <- trf_expr else_e
                return (L loc (HsIf cond_t then_t expr_t))
          otherwise -> throwError (text "Wrong arity: `if' needs 3 arguments")
    | sym == "let" = 
        do
         case rest of
           [(PList _ bindings ("","")), expr] -> 
               do
                 bindings_t <- trf_letbindings bindings
                 expr_t <- trf_expr expr
                 return (L loc (HsLet (HsValBinds (cvBindGroup bindings_t))
                                      expr_t))
           otherwise -> throwError (text "wrong arity: `let' needs 2 arguments")
    | sym == "case" = 
        case rest of 
          (expr_e:as) -> do
                   expr_t <- trf_expr expr_e
                   as_t <- mapM trf_match as
                   return (L loc (HsCase expr_t
                                         (mkMatchGroup as_t)))
          otherwise -> throwError (text "wrong arity, case")
    | sym == "lambda" = case rest of 
                          [(PList _ pats ("","")), expr] -> do 
                                    expr_t <- trf_expr expr
                                    pats_t <- mapM trf_pattern pats
                                    return (L loc (HsLam (mkMatchGroup [(mkSimpleMatch pats_t expr_t)])))
                          _ -> throwError (text "Wrong arity: `lambda' needs 2 arguments")
    | sym == "::" = 
        case rest of 
          [expr, type_e] -> do 
                    expr_t <- trf_expr expr
                    type_t <- trf_sig_type type_e
                    return (L loc (ExprWithTySig expr_t type_t))
          otherwise -> throwError (text "Wrong arity: `::' needs 2 arguments")
    | sym == "[]" = do
        exprs_t <- mapM trf_expr rest
        return (L loc (ExplicitList placeHolderType exprs_t))
    | sym == "," = do
        exprs_t <- mapM trf_expr rest
        return (L loc (ExplicitTuple exprs_t BasicTypes.Boxed))
#warning envlet support in expression removed for the moment
{--    | sym == "envlet" = case rest of
                            macrofun:expr:[] -> do 
                                      macrofun_t <- trf_expr macrofun
                                      env <- askEnv
                                      return (L loc (HsSpliceE (HsLskSplice unqualSplice 
                                                                macrofun_t
                                                                env
                                                                expr
                                                        ))) --}
    | sym == "app" = trf_exprs_app rest
    | otherwise = trf_exprs_app full

trf_expr_prim (PList loc full ("","")) = trf_exprs_app full
trf_expr_prim p = throwErrorAt (ploc p) (text ("Exprs transform failed for " ++ (show p)))

trf_exprs_app :: [ParseTree] -> TransformationMonad (LHsExpr RdrName)
trf_exprs_app = trf_exprs_ . reverse

trf_exprs_ :: [ParseTree] -> TransformationMonad (LHsExpr RdrName)
trf_exprs_ (x:[]) = trf_expr x
trf_exprs_ (x:xs) = do
  rest_t <- trf_exprs_ xs
  first_t <- trf_expr x
  return (noLoc (HsApp rest_t first_t))

-------------------------------------------------------------------------------------
--- Pattern transformation into LPat  
-------------------------------------------------------------------------------------

trf_pattern :: ParseTree -> TransformationMonad (LPat RdrName)
trf_pattern pt = do
  env <- askEnv
  (envPatTable env) pt trf_pattern trf_pattern_prim

trf_pattern_prim :: ParseTree -> TransformationMonad (LPat RdrName)
trf_pattern_prim sym@(PSym loc id)
  | id == "_" = return (L loc (WildPat placeHolderType))
  | Just int <- convertNumber id = return (L loc (mkNPat (mkHsIntegral int placeHolderType) Nothing))
  | Just char <- convertChar id = return (L loc (LitPat (HsChar char)))
  | Just rational <- convertRational id = return (L loc (mkNPat (mkHsFractional rational placeHolderType) Nothing))
  | Just string <- convertString id = return (L loc (LitPat (HsString (mkFastString string))))
  | isUpId id = return (L loc (ConPatIn (mkLRdrName2 undefined dataName sym)
                                            (PrefixCon [])))
  | otherwise = return (L loc (VarPat (mkRdrName2 varName undefined sym)))

trf_pattern_prim p@(PList loc (sym@(PSym sloc id):rest) ("",""))
  | id == "@" = case rest of 
                   assym@(PSym iloc id):p:[] ->
                     if (isLowId id) then 
                         do 
                           p_t <- trf_pattern p
                           return (L loc (AsPat (mkLRdrName2 varName undefined assym) p_t))
                         else
                             throwError (text "as name must be a lowid")
                   otherwise -> throwError (text "Wrong arity: `@' needs two arguments")
  | id == "~" = case rest of 
                   p:[] -> do
                     p_t <- trf_pattern p
                     return (L loc (LazyPat p_t))
                   otherwise -> throwError (text "wrong arity: `~' needs one argument")
--  | id == "!" = case rest of 
--                   p:[] -> undefined
--  | id == "::" = case rest of 
--                    p:t:[] -> throwError ( "unimplemented :: pattern")
  | id == "[]" = do
      ps_t <- mapM trf_pattern rest
      return (L loc (ListPat ps_t placeHolderType))
  | id == "," = do
      ps_t <- mapM trf_pattern rest
      return (L loc (TuplePat ps_t BasicTypes.Boxed placeHolderType))
  | id == "Con" = case rest of
                    (sym@(PSym sloc id):rest) ->
                        if (isUpId id) then -- won't match ,[] !! FIXMELSK
                            do
                              ps_t <- mapM trf_pattern rest
                              return (L loc (ConPatIn (mkLRdrName2 undefined dataName sym)
                                                          (PrefixCon ps_t)))
                        else
                            throwError (text "Error in explicit prefix constructor")
                    otherwise -> throwError (text "Error in explicit prefix constructor")
  | isUpId id = do
       ps_t <- mapM trf_pattern rest
       return (L loc (ConPatIn (mkLRdrName2 undefined dataName sym)
                                   (PrefixCon ps_t)))
  | otherwise = throwError (text ("Unknown pattern" ++ (show p)))

trf_pattern_prim p = throwError (text ("Unknown pattern" ++ (show p)))


-------------------------------------------------------------------------------------
--- Type transformation into LHsType  
-------------------------------------------------------------------------------------

trf_type :: ParseTree -> TransformationMonad (LHsType RdrName)
trf_type pt = do
  env <- askEnv
  (envTypeTable env) pt trf_type trf_type_prim

trf_type_prim :: ParseTree -> TransformationMonad (LHsType RdrName)
trf_type_prim c@(PSym loc var) = return (L loc (HsTyVar (mkRdrName2 tvName tcClsName c)))

trf_type_prim (PList loc full@((PSym sloc sym):rest) ("",""))
  | sym == "->" = trf_type_chain (flip HsFunTy) rest
  | sym == "[]" = case rest of 
                    t1:[] -> do
                              t1_t <- trf_type t1
                              return (L loc (HsListTy t1_t))
                    otherwise -> throwError (text ("Wrong arity: `[]' needs one argument"))
  | sym == "," = 
      do
        types_t <- mapM trf_type rest
        return (L loc (HsTupleTy BasicTypes.Boxed types_t))
  -- Bang Type
  | sym == "!" = 
      case rest of
        type_e:[] -> do
                  type_t <- trf_type type_e
                  return (L loc (HsBangTy HsStrict type_t))
        otherwise -> throwError (text "Wrong arity: bangtype needs one argument")
  -- Type with context
  | sym == "=>" = case rest of
                   [(PList loc context ("","")), restricted_type]  -> do
                       context_t <- case context of
                                      (PList _ _ _):_ -> mapM trf_context context
                                      otherwise -> mapM trf_context [PList loc context noPP]
                       restricted_type_t <- trf_type restricted_type
                       return (L loc (HsForAllTy Implicit 
                                                 []
                                                 (L loc context_t)
                                                 restricted_type_t))
  | sym == "forall" = case rest of                                                         -- is that "forall" inconstant with the rest of charcrap keywords?
                        [(PList vloc tyvars ("","")), 
                         (PList loc context ("","")), 
                         restricted_type] -> do
                                  context_t <- mapM trf_context context
                                  restricted_type_t <- trf_type restricted_type
                                  return (L loc (HsForAllTy Explicit 
                                                 (map symToUTV tyvars)
                                                 (L loc context_t)
                                                 restricted_type_t))
  | sym == "app" = trf_type_app rest
  | otherwise = trf_type_app full

trf_type_prim (PList _ s ("","")) = trf_type_app s
trf_type_prim p = throwErrorAt  (ploc p) (text ("Unknown type form " ++ (show p)))


trf_type_app :: [ParseTree] -> TransformationMonad (LHsType RdrName)
trf_type_app = (trf_type_chain HsAppTy) . reverse

-- isn't that fold below?
trf_type_chain con (x:[]) = trf_type x
trf_type_chain con (x:xs)  = do
  xs_t <- trf_type_chain con xs
  x_t <- trf_type x
  return $ noLoc $ con xs_t x_t

--- Signature type
trf_sig_type parsetree = do 
  type_t <- trf_type parsetree
  return (L (ploc parsetree) (mkImplicitHsForAllTy (noLoc []) type_t))

--- Context transformation
trf_context (PList loc (con:arg) ("","")) = do
  types_t <- mapM trf_type arg
  return (L loc (HsClassP (mkRdrName2 (error "Not a classname in context") tcClsName  con)
                          types_t))


---

symToUTV s@(PSym loc _) = L loc (UserTyVar (mkRdrName2 tvName (error "Not a type variable") s))

---

-- Stolen from RdrHsSyn.lhs - This is pretty much code duplication, no
-- idea what to do about it. It would require an abstract ParserMonad, maybe.

-- This converts a HsType into a ConDecl, constructor declaration.
-- The requirements for this to work is that the type is a series of Type Applications with 
-- a TyVar at the innermost left.

mkPrefixConI  :: Located (HsType RdrName) -> TransformationMonad (LConDecl RdrName)
mkPrefixConI ty
 = split ty []
 where
   tyConToDataCon loc tc  | isTcOcc (rdrNameOcc tc)  = return (L loc (setRdrNameSpace tc srcDataName))
                          | otherwise = throwError (text "Not a constructor..")
   split (L _ (HsAppTy t u)) ts = split t (u : ts)
   split (L l (HsTyVar tc))  ts = do data_con <- tyConToDataCon l tc
                                     return (noLoc (cConDecl data_con Explicit [] (noLoc []) (PrefixCon ts) ResTyH98))
   split (L l _) _ 		= throwError (text "parse error in data/newtype declaration")

---

checkDictTy (L spn ty) = check ty []
  where
  check (HsTyVar t) args | not (isRdrTyVar t) 
 	= return (L spn (HsPredTy (HsClassP t args)))
--        = throwError ( "Args has " ++ show (length args) ++ showSDoc (ppr args))
  check (HsAppTy l r) args = check (unLoc l) (r:args)
  check _ _ = throwError (text "Malformed instance type")

checkInstTypeT (L l t)
  = case t of
	HsForAllTy exp tvs ctxt ty -> do
		dict_ty <- checkDictTy ty
	      	return (L l (HsForAllTy exp tvs ctxt dict_ty))
        HsParTy ty -> checkInstTypeT ty -- HsParTy = Parenthesized TYPE!
	ty ->   do dict_ty <- checkDictTy (L l ty)
	      	   return (L l (HsForAllTy Implicit [] (noLoc []) dict_ty))

-- An SD is a definitional type of the form (TypeConstructor v1 v2 ..) or (> [context] (TypeConstructor v1 v2))
-- Isn't that a bit of code duplication from trf_type?
trf_def_type :: ParseTree -> TransformationMonad ((LHsContext RdrName), SimpleType)
trf_def_type s@(PSym loc _) = trf_def_type (PList loc [s] noPP)
trf_def_type s@(PList loc ((PSym _ sym):rest) ("",""))
    | sym /= "=>" = trf_def_type (PList loc [(PSym loc "=>"),
                                             PList loc [] noPP,
					     s]
                                            noPP) 
    | otherwise = case rest of
                    [(PList cloc contexts ("","")), simple_type] -> do
                              contexts_t <- mapM trf_context contexts
                              simple_type_t <- trf_simpleType simple_type
                              return (L cloc contexts_t, simple_type_t)
                    otherwise -> throwErrorAt  (ploc s) (text "Malformed definitional type")

-- A simple constructor type Foo v1 v2 v3 is transformated to (name, vars)
trf_simpleType :: ParseTree -> TransformationMonad SimpleType
trf_simpleType s@(PSym loc _) = trf_simpleType (PList loc [s] noPP)
trf_simpleType (PList loc (head:vars) ("","")) = return ((mkLRdrName2 (error "Context class not a class name") tcClsName head), map symToUTV vars)

-------------------------------------------------------------------------------------
--- Top Level transformation into LHsDecl
-------------------------------------------------------------------------------------

trf_decl :: ParseTree -> TransformationMonad [LHsDecl RdrName]
trf_decl pt = do
  env <- askEnv
  let ks = ((envDeclTable env) :: (ParseTree -> (ParseTree -> a) -> (ParseTree -> a) -> a))
  ks pt trf_decl trf_decl_prim

trf_constructor constr = do 
  constr_t <- trf_type constr
  mkPrefixConI constr_t

trf_decl_prim :: ParseTree -> TransformationMonad [LHsDecl RdrName]
trf_decl_prim (PList _ (s@((PList loc ((PSym sloc sym):rest) ("","")):ds')) ("",""))
  | sym == "define" = case rest of
                        [bind, expr] -> do
                                  bind_t <- trf_EBinder bind expr
				  let this = L loc $ ValD bind_t
				  addEvalCtxDecl this
                                  ds_t <- trf_decl ds
                                  return (this:ds_t)
                        full@[bind, expr, sig] -> do 
                                  this <- trf_ETFunbinder (PList loc full noPP)
				  mapM addEvalCtxDecl this
                                  rest <- trf_decl ds
                                  return (this++rest :: [LHsDecl RdrName])
                        otherwise -> throwErrorAt loc (text ("Malformed define declaration: " ++ (show s)))
  | sym == "defwithsig" = do 
                         this <- trf_TEFunbinder (PList loc rest noPP)
			 mapM addEvalCtxDecl this
                         ds_t <- trf_decl ds
                         return (this++ds_t)
  | sym == "deftype" = case rest of
                         [sctype, type_e] -> do
                                   (name_t, tyvars_t) <- trf_simpleType sctype
                                   type_t <- trf_type type_e
				   let this = (L loc (TyClD (cTySynonym name_t tyvars_t type_t)))
				   addEvalCtxDecl this
                                   ds_t <- trf_decl ds
                                   return (this:ds_t)
                         otherwise -> throwErrorAt loc (text "Malformed type definition")
  | sym == "defdata" = case rest of 
                         sdtype:constrs -> do
                                   (contexts_t, (name_t, tyvars_t)) <- trf_def_type sdtype
                                   constrs_t <- mapM trf_constructor constrs
				   let this = (L loc (TyClD (cmkTyData DataType (contexts_t, name_t, tyvars_t) Nothing constrs_t Nothing)))
				   addEvalCtxDecl this
                                   ds_t <- trf_decl ds
                                   return (this:ds_t)
                         otherwise -> throwErrorAt loc (text "Malformed defdata declaration")
  | sym == "defnewtype" = case rest of 
                         [sdtype, constr] -> do
                                   (contexts_t, (name_t, tyvars_t)) <- trf_def_type sdtype 
                                   constr_t <- trf_constructor constr
				   let this = (L loc (TyClD (cmkTyData NewType (contexts_t, name_t, tyvars_t) Nothing [constr_t] Nothing)))
				   addEvalCtxDecl this
                                   ds_t <- trf_decl ds
                                   return (this:ds_t)
                         otherwise -> throwErrorAt loc (text "Malformed defnewtype declaration")
  | sym == "defclass" = case rest of 
                          sdtype:teBinds -> do
                                    (contexts_t, (name_t, tyvars_t)) <- trf_def_type sdtype
                                    decls_t <- mapM trf_TEFunbinder teBinds 
				    let (binds, sigs) = ccvBindsAndSigs (toOL (concat decls_t))
#warning ATS?
				        this = L loc (TyClD (cmkClassDecl (contexts_t, name_t, tyvars_t)
                                                             []
                                                             sigs 
                                                             binds))
				    addEvalCtxDecl this
                                    ds_t <- trf_decl ds
                                    return (this:ds_t)
                          otherwise -> throwErrorAt loc (text "Empty defclass definition")
  | sym == "definstance" = case rest of 
                             insttype:etBinds -> do
                                       insttype_t@(L _ (HsForAllTy _ _ _ _)) <- trf_sig_type insttype
				       asPredType_t <- checkInstTypeT insttype_t 
                                       decls_t <- mapM trf_ETFunbinder etBinds
				       let (binds, sigs, ats, _) = cvBindsAndSigs (toOL (concat decls_t))
				           this = L loc (InstD (InstDecl asPredType_t binds sigs ats))
				       addEvalCtxDecl this
                                       ds_t <- trf_decl ds
-- For GHC > 6.6
--                                       return ((let (binds, sigs, ats, _) = cvBindsAndSigs (toOL (concat decls_t))
--                                                in L loc (InstD (InstDecl asPredType_t binds sigs ats))):ds_t)
                                       return (this:ds_t)
                             otherwise -> throwErrorAt loc (text "Empty definstance declaration")
  | sym == "envlet" = case rest of
				  macrofun:exprs -> do
					evalCtx <- askEvalCtx
					hsc_env <- askHscEnv
					transformerExpr <- trf_expr macrofun
					lskEnvtransformer <- eval transformerExpr evalCtx hsc_env
					s <- askTrfState
					new_env <- lift $ lskEnvtransformer (ts_lskenv s)
					this <- withTrfState (s { ts_lskenv = new_env }) $ trf_decl (PList loc exprs  noPP)
					ds_t <- trf_decl ds
					return (this ++ ds_t)
				  otherwise -> throwErrorAt loc (text "Empty macrolet declaration")
  | sym == "defenv" = case rest of 
                        [macrofun] -> trf_decl (sPList [PList loc ((PSym sloc "envlet"):macrofun:ds') noPP])
                        otherwise -> throwErrorAt loc (text "Malformed defenv")
  | otherwise = throwErrorAt (ploc (head s)) (text ("Unknown declaration head " ++ (show (head s))) )
  where
    sPList lst = PList noSrcSpan lst noPP
    ds = sPList ds'

trf_decl_prim (PList _ [] ("","")) = return [] -- we do not care
trf_decl_prim pt = throwErrorAt (ploc pt) (text ("Unknown declaration" ++ (show pt)))

---

type LHsModule a = Located (HsModule a)

ensure_lskPrelude imports module_name
  | not (pt_sym module_name == "LskPrelude") =
      case find (\elem -> let (ImportDecl name _ _ _ _ _) = unLoc elem
	       		  in (unLoc name) == lskPreludename)
	   imports of
	Nothing -> (L noSrcSpan (ImportDecl (L noSrcSpan lskPreludename)
					    Nothing
			      		    False
					    False
					    Nothing
					    Nothing)):imports
	Just _ -> imports
  | otherwise = imports
  where lskPreludename = (mkModuleName "LskPrelude")

trf_module :: [ParseTree] -> Bool -> TransformationMonad (LHsModule RdrName)
trf_module (defmodule:decls) onlyheader =
    case defmodule of
      (PList loc [(PSym _ "defmodule"),
                  modname, 
                  export_list, 
                  (PList _ import_list noPP)] 
              ("","")) -> 
          do
            modulename_t <- trf_modulename modname
            imports_maybes_t <- mapM trf_import import_list
	    let imports_t = catMaybes imports_maybes_t
	    setEvalCtxImports imports_t
--	    lift $ putStrLn ("setting x numbers of imports: " ++ (show $ length imports_t))
--	    evalCtx <- askEvalCtx
--	    lift $ putStrLn ("having x numbers of imports: " ++ (show $ length $ fst evalCtx))
            exports_t <- if onlyheader then  -- onlyheader=True -> parse only imports and module name
                             return Nothing
                         else 
                             trf_exports export_list
					 
            decls_t <- if onlyheader then 
                           return []
                       else 
                           trf_decl (PList noSrcSpan decls noPP)
            return (L loc (cHsModule (Just (L (ploc modname) modulename_t))
                           exports_t
#warning Ensure LskPrelude
			 --  (ensure_lskPrelude imports_t modname)
			   imports_t
                           decls_t
                           Nothing))
      otherwise -> throwError (text "First element of parse tree not a well-formed defmodule statement.")

trf_module _ _ = throwError (text "Parse result not a list")

-------------------------------------------------------------------------------------
--- Entry Points
-------------------------------------------------------------------------------------

liskell_transform entry content loc = do
   ptree <- (case parseLiskell (lexLiskell content loc) of
               LPMOk a -> return a
	       LPMFailed s m -> throwErrorAt s m)
   entry ptree

liskell_transform_source = liskell_transform (flip trf_module False)
liskell_transform_header_only = liskell_transform (flip trf_module True)
liskell_transform_expr = liskell_transform (\[pt] -> trf_expr pt)

seedLskTrfEnv :: IO LskEnvironment
seedLskTrfEnv = do
  return $ LskEnv idcont idcont idcont idcont
  where idcont = \pt ks kf -> kf pt  -- identitiy continuation
