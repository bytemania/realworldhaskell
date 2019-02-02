module PrettyStub where

import Data.Bits
import Data.Char
import Numeric

import SimpleJSON

data Doc = ToBeDefined deriving Show

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined

pointyString :: String -> Doc
pointyString s = enclose '"' '"' (hcat (map oneChar s))

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <|> x <|> char right

(<|>) :: Doc -> Doc -> Doc
a <|> b = undefined

char :: Char -> Doc
char c = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
                Just r -> text r
                Nothing | mustEscape c -> hexEscape c
                        | otherwise    -> char c
    where mustEscape c = c  < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\', b])

smallHex :: Int -> Doc
smallHex x = text "\\u" <|> text (replicate (4 - length h) '0') <|> text h
    where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <|> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
    where d = ord c

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close . fsep . punctuate (char ',') . map item

fsep :: [Doc] -> Doc
fsep xs = undefined

renderJValue (JArray ary) = series '[' ']' renderJValue ary
renderJValue (JObject obj) = series '{' '}' field obj
    where field (name, val) = string name <|> text ": " <|> renderJValue val



