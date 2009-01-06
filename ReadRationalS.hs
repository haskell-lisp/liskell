module ReadRationalS where
import Data.Char        ( isUpper, isAlphaNum, isSpace, ord, isDigit )
import Data.Ratio       ( (%) )

readRationalS :: ReadS Rational -- NB: doesn't handle leading "-"
readRationalS r = do
     (n,d,s) <- readFix r
     (k,t)   <- readExp s
     return ((n%1)*10^^(k-d), t)
 where
     readFix r = do
        (ds,s)  <- lexDecDigits r
        (ds',t) <- lexDotDigits s
        return (read (ds++ds'), length ds', t)

     readExp (e:s) | e `elem` "eE" = readExp' s
     readExp s                     = return (0,s)

     readExp' ('+':s) = readDec s
     readExp' ('-':s) = do (k,t) <- readDec s
                           return (-k,t)
     readExp' s       = readDec s

     readDec s = do
        (ds,r) <- nonnull isDigit s
        return (foldl1 (\n d -> n * 10 + d) [ ord d - ord '0' | d <- ds ],
                r)

     lexDecDigits = nonnull isDigit

     lexDotDigits ('.':s) = return (span isDigit s)
     lexDotDigits s       = return ("",s)

     nonnull p s = do (cs@(_:_),t) <- return (span p s)
                      return (cs,t)
