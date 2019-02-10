module Monomorphism where

myShow value = show value

myShow2 :: (Show a) => a -> String
myShow2 = show

