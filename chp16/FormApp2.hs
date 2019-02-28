module FormApp2 where

import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Applicative hiding (many, (<|>))
import Numeric

p_hex :: CharParser () Char
p_hex = hexify <$> (char '%' *> hexDigit) <*> hexDigit
    where  hexify a b = toEnum . fst . head . readHex $ [a,b]

urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

p_char :: CharParser () Char
p_char = oneOf urlBaseChars
     <|> (char '+' >> return ' ')
     <|> p_hex

a_char = oneOf urlBaseChars
     <|> (' ' <$ char '+')
     <|> p_hex

p_pair_app1 = liftM2 (,) (many1 p_char) (optionMaybe (char '=' >> many p_char))

a_pair :: CharParser () (String, Maybe String)
a_pair = liftA2 (,) (many1 a_char) (optionMaybe (char '=' *> many a_char))



