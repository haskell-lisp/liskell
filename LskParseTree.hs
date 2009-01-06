module LskParseTree where
import SrcLoc
import FastString
import Data.List
import qualified Numeric
import ReadRationalS

data ParseTree = PSym { pt_loc :: SrcSpan, pt_sym :: String }
               | PList { pt_loc :: SrcSpan, pt_list :: [ParseTree], pt_pp :: (String, String) }

instance Show ParseTree where
    show (PSym _ s) = s
    show (PList _ l (pe, ps)) = pe ++ "(" ++ (tail (concat (map (\pt -> " " ++ show pt)
								l))) ++ ")" ++ ps

ploc = pt_loc

instance Eq ParseTree where
    (==) (PSym _ s1) (PSym _ s2) = s1 == s2
    (==) (PList _ l1 pp1) (PList _ l2 pp2) = l1 == l2 && pp1==pp2
    (==) a b = False

noPP = ("", "") -- No pre- or postfix

instance Show SrcSpan where
    show a = "" -- we might want to improvide this

class Parseable a where
    toParseTree :: a -> ParseTree

instance Parseable ParseTree where
    toParseTree = id

instance Parseable Integer where
    toParseTree int = (PSym noSrcSpan (show int))

instance Parseable Int where
    toParseTree int = (PSym noSrcSpan (show int))

instance Parseable Char where
    toParseTree char = (PSym noSrcSpan ("#\\" ++ [char]))

--instance Parseable String where
--    toParseTree str = (PString noSrcSpan str)

instance Parseable a => Parseable [a] where
    toParseTree ps = (PList noSrcSpan (map toParseTree ps) noPP)

macroSrcSpan  = mkGeneralSrcSpan (mkFastString ("<no location info - from Liskell Macro>"))

-- Trivial helpers that might be useful to external macros

parseQual str =
    let rstr = reverse str
    in case (dropWhile (/= '.') rstr, takeWhile (/= '.') rstr) of
	 ("", _) -> ("", str)                               -- str doesn't contain a dot
	 (_, "")  -> ("", str)                              -- str ends with a dot such as "abc.kuh." or simply "."
	 (qual, sym) -> (reverse (tail qual), reverse sym)  -- found qualified name

parseOrig str =
    case (takeWhile (/= ':') str, dropWhile (/= ':') str) of
      ("", _) -> wrappedParseQual "" str                -- str starts with ":"
      (_, "") -> wrappedParseQual "" str                -- str doesn't contain ":"
      (qual, str') -> case parseQual (tail str') of     -- str contains ":"
			("", sym) -> ("","", str)        -- but no module name found, hence no orig. such as "a:a"
			(mod, sym) -> (qual, mod, sym)   -- found original, qualified module name, and symbol
    where wrappedParseQual qual str =
	      let (modname, sym) = parseQual str
	      in (qual, modname, sym)

convertDec str =
    case Numeric.readDec str of
      [(int, "")] -> Just int
      _ -> Nothing

convertHex str
    | (isPrefixOf "0x" str || isPrefixOf "0X" str) 
	= case Numeric.readHex (drop 2 str) of 
	    [(int, "")] -> Just int
	    _ -> (error "Invalid hex number")
    | otherwise = Nothing

convertOct str
    | isPrefixOf "0o" str || isPrefixOf "0o" str 
	= case Numeric.readOct (drop 2 str) of 
	    [(int, "")] -> Just int
	    _ -> (error "Invalid octal number")
    | otherwise = Nothing

convertNumber str = (convertOct str) `orElse'` (convertHex str) `orElse'` (convertDec str)

convertChar str
    | isPrefixOf "#\\" str && length str == 3 = Just (str !! 2)
    | otherwise = Nothing

convertRational str =
    case readRationalS str of
	[(rat, "")] -> Just rat
	_ -> Nothing

convertString str
 | isPrefixOf "\"" str && isSuffixOf "\"" str = Just (tail (take ((length str) - 1) str))  -- FIXMELSK this looks a bit ugly
 | otherwise = Nothing


orElse' :: Maybe a -> Maybe a -> Maybe a
orElse' a b =
    case a of
      (Just _) -> a
      Nothing -> b
