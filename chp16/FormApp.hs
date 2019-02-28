module FormApp where

import Text.ParserCombinators.Parsec
import Control.Monad
import Numeric

p_hex :: CharParser () Char
p_hex = do
     char '%'
     a <- hexDigit
     b <- hexDigit
     let ((d, _):_) = readHex [a,b]
     return . toEnum $ d

