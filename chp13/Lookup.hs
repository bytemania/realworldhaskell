module Lookup where

myLookup :: Eq a => a -> [(a,b)] -> Maybe b
myLookup _ [] = Nothing
myLookup key ((thiskey, thisval):rest)
    | key == thiskey = Just thisval
    | otherwise = myLookup key rest
