module ToUpperLazy4 where

import Data.Char (toUpper)

main :: IO ()
main = interact ((++) "Your data, in uppercase, is:\n\n" . map toUpper )
