module PrettyStub where

{-# LANGUAGE TypeSynonymInstances #-}

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

type JSONError = String

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
    toJValue = id
    fromJValue = Right

instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = Right b
    fromJValue _ = Left "not a JSON boolean"

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _ = Left "Not a JSON number"

instance JSON Int where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Integer where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Double where
    toJValue = JNumber
    fromJValue = doubleToJValue

instance (JSON a) => JSON [a] where
    toJValue = undefined
    fromJValue = undefined

instance (JSON a) => JSON [a] where
    toJValue = undefined
    fromJValue = undefined




