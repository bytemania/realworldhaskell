module TypeConstraint where

data OrdStack a = Bottom | Item a (OrdStack a) deriving (Show)

isInscreasing :: (Ord a) => OrdStack a -> Bool
isInscreasing (Item a rest@(Item b _))
    | a < b = isInscreasing rest
    | otherwise = False
isInscreasing _ = True

push :: a -> OrdStack a -> OrdStack a
push a s = Item a s

