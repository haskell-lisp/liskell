module LskTransformationMonad where
import LskParseTree
import HsSyn
import RdrName
import SrcLoc
import Outputable
import ErrUtils		( Message )
import MonadUtils
import HscTypes

-------------------------------------------------------------------------------------
--- TransformationMonad
-------------------------------------------------------------------------------------

--- The Transformation Monad is basically a combination of the
--- Error+Reader Monad with an IO Monad

data LskEnvironment = LskEnv 
    --                       SUCCESS CONT.       FAILURE CONT.
    (forall a.(ParseTree -> (ParseTree -> a) -> (ParseTree -> a) -> a)) -- ExprTable
    (forall a.(ParseTree -> (ParseTree -> a) -> (ParseTree -> a) -> a)) -- PatTable
    (forall a.(ParseTree -> (ParseTree -> a) -> (ParseTree -> a) -> a)) -- TypeTable
    (forall a.(ParseTree -> (ParseTree -> a) -> (ParseTree -> a) -> a)) -- DeclTable

-- Transformation Environment

type LskEnvironmentTransformer = LskEnvironment -> IO LskEnvironment

envExprTable (LskEnv e _ _ _) = e
envPatTable (LskEnv _ p _ _) = p
envTypeTable (LskEnv _ _ t _) = t
envDeclTable (LskEnv _ _ _ d) = d

-- The Monad itself

type Variables = String

data TransformationState = TransformationState { 
      ts_lskenv :: LskEnvironment,
      ts_hsc_env :: HscEnv,
      ts_freshvars :: [Variables],
      ts_evalctx :: ([LImportDecl RdrName], [LHsDecl RdrName])
    }

data TransformationError = TrErr SrcSpan Message

data TransformationMonad a = TM { runTM :: TransformationState -> (IO (Either TransformationError (TransformationState,a))) }

instance Monad TransformationMonad where
    m >>= k = TM $ \s -> do
      a <- runTM m s
      case a of 
        Left l -> return (Left l)
        Right (s',r) -> runTM (k r) s'
    return a = TM $ \s -> return (Right (s,a))

instance MonadIO TransformationMonad where
    liftIO m = TM $ \s -> do
               a <- m
	       return (Right (s,a))
   

throwError m     = TM $ \_ -> return (Left (TrErr noSrcSpan m))
throwErrorAt s m = TM $ \_ -> return (Left (TrErr s m))

m `catchError` h = TM $ \s -> do
		     a <- runTM m s
		     case a of
		       Left  l -> runTM (h l) s
		       Right (s,r) -> return (Right (s,r))

askVars = TM $ \s -> return (Right (s, ts_freshvars s))
askEnv  = TM $ \s -> return (Right (s, ts_lskenv s))
askHscEnv  = TM $ \s -> return (Right (s, ts_hsc_env s))
askEvalCtx  = TM $ \s -> return (Right (s, ts_evalctx s))


getsTM = TM $ \s -> return (Right (s, s))
setsTM s = TM $ \_ -> return (Right (s, ()))

setEvalCtxImports i  = TM $ \s -> 
  return $ Right (s { ts_evalctx = (i, (snd $ ts_evalctx s)) }, ())

addEvalCtxDecl d = TM $ \s -> do
  let oldeval = ts_evalctx s
  return $ Right (s { ts_evalctx = (fst oldeval, d:(snd oldeval)) }, ())

genSym :: TransformationMonad ParseTree
genSym = do
   TM $ \ts -> 
       let vars = ts_freshvars ts
       in return $ Right (ts { ts_freshvars = (tail vars) },
			  PSym noSrcSpan (head vars))

newFreshVarStream prefix = map (((toEnum 0:prefix) ++) . show) [1..]

withTrfState s' m = TM $ \s -> (runTM m) s'
askTrfState = TM $ \s -> return (Right (s, s))
withEnvTrf f m = TM $ \s -> (runTM m) (s { ts_lskenv = f (ts_lskenv s) })

lift m = TM $ \s -> do
           a <- m
           return (Right (s,a))
