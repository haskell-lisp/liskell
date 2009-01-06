{
module ParseLiskell(parseLiskell,LiskellParserMonad(..)) where
import LexLiskell 
import SrcLoc
import FastString
import StringBuffer
import LskParseTree
import ErrUtils		( Message )
import Outputable
}

%name parseLiskell Exps
%tokentype { Token  }
%monad { LiskellParserMonad } { thenLPM } { returnLPM }
%token
'('        { TOParent _ _}
')'        { TCParent _ _}
sym        { TSym _ _ }

%%

Exps : Exps1 { reverse $1 }

Exps1 : {- empty -}    { [] }
    | Exps1 Exp       { $2:$1 } 
       
Exp : Atom            { $1 } 
    | List            { $1 }

Atom : sym           { PSym (locSS $1) (idT $1) }

List : '(' Exps ')'  { PList (mkSrcSpan (loc $1) (loc $3))
                             $2 
		             (preSym $1, postSym $3) } 

{

data LiskellParserMonad a = LPMOk a | LPMFailed SrcSpan Message

thenLPM :: LiskellParserMonad a -> (a -> LiskellParserMonad b) -> LiskellParserMonad b
m `thenLPM` k =
   case m of
       LPMOk a -> k a
       LPMFailed s m -> LPMFailed s m

returnLPM :: a -> LiskellParserMonad a
returnLPM a = LPMOk a

failLPM :: String -> LiskellParserMonad a
failLPM s  = LPMFailed noSrcSpan (text s)

catchLPM :: LiskellParserMonad a -> (SrcSpan -> Message -> LiskellParserMonad a) -> LiskellParserMonad a
catchLPM m k =
   case m of
      LPMOk a -> LPMOk a
      LPMFailed s m -> k s m

locSS token = mkSrcSpan point point
            where point = loc token
		          
happyError tokens = LPMFailed (mkSrcSpan (loc (head tokens)) (loc (head tokens))) (text ("Parse Error: " ++ (show (printLiskellToken (head tokens)))))
}
