module PrettyJSON (
        renderJValue
    ) where

import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

import SimpleJSON (JValue(..))
import Pretiffy (Doc, (<|>), char, double, fsep, hcat, punctuate, text, compact, pretty)

renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str

