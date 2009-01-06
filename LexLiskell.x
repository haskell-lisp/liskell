{
--								-*-haskell-*-
-- ---------------------------------------------------------------------------
-- Liskell
-- Lexer for Liskell
--
-- Author(s): Clemens Fruhwirth <clemens@endorphin.org>
-- ---------------------------------------------------------------------------

module LexLiskell(lexLiskell, Token(..),printLiskellToken) where
import SrcLoc
import StringBuffer
import FastString
import Numeric
import Util
}


$digit = 0-9			-- digits
$hex = [0-9 a-f A-F] 		-- hex
$octal = 0-7			-- octal
$lowerchars = [a-z]		-- lowerchars
$upperchars = [A-Z]		-- upperchars
$liskellsymbols = [\:\,\_\`\']      -- additional symbols allowed by Liskell
$haskellsymbols = [\!\#\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\[\]\_\,\:] -- as in Page 8 Haskell 98 Report 
$symbols = [$liskellsymbols $haskellsymbols]
$alpha = [[A-Z][a-z]]
$idchars = [$alpha $symbols $digit]
@id = $idchars+
@upid = $upperchars $idchars*
@qualid = (@upid [\.])+ @id

tokens :-

  $white+				;
  ";".*				        ;
  $idchars* "("				{ \p s -> TOParent p (init s) }
  ")" $idchars*				{ \p s -> TCParent p (tail s) }

  -- Strings
  \"[^\"]*\"				{ TSym } -- FIXME. what about control chars?

  -- Everything else is symbol not including symbol separators (whitespace, parenthesis and comments) 
  [^\ \(\)\;]+				{ TSym }
{

-- The token type:
data Token     =  TOParent { loc :: SrcLoc, preSym :: String }
		| TCParent { loc :: SrcLoc, postSym :: String }
		| TSym { loc :: SrcLoc, idT :: String }
		| TString { loc :: SrcLoc, stringT :: String }
		deriving (Eq,Show)

printLiskellToken (TOParent _ pre) = pre ++ "("
printLiskellToken (TCParent _ post) = ")" ++ post
printLiskellToken (TSym _ id) = id
printLiskellToken (TString _ str) = show str

--------------------------------------------------------------------------------
-- The input type
--------------------------------------------------------------------------------
type AlexInput = (SrcLoc, 	-- current position,
		  StringBuffer)	-- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,buf) = prevChar buf '\n'

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (loc,s)
  | atEnd s   = Nothing
  | otherwise = c `seq` loc' `seq` s' `seq`
		--trace (show (ord c)) $
		Just (c, (loc', s'))
  where (c,s') = nextChar s
        loc'   = advanceSrcLoc loc c

alexStartPos = mkSrcLoc (mkFastString "foo") 1 0

alexScanTokens startpos str = go (startpos,str)
  where go inp@(pos,strbuf) =
	  case alexScan inp 0 of
		AlexEOF -> []
		AlexError _ -> error "lexical error"
		AlexSkip  inp' len     -> go inp'
		AlexToken inp' len act -> act pos (lexemeToString strbuf len) : go inp'

instance Show SrcLoc where
	show s = ""

lexLiskell s l = alexScanTokens l s

main = do
  s <- getContents
  sb <- stringToStringBuffer s
  print (alexScanTokens alexStartPos sb)
}