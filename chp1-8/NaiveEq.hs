module NaiveEq where

data Color = Red | Green | Blue deriving (Eq, Ord)

colorEq :: Color -> Color -> Bool
colorEq Red Red = True
colorEq Green Green = True
colorEq Blue Blue = True
colorEq _ _ = False

stringEq :: [Char] -> [Char] -> Bool
stringEq [] [] = True
stringEq (x:xs) (y:ys) = x == y && stringEq xs ys

data CannotShow = CannotShow deriving Show

data CannotDeriveShow = CannotDeriveShow CannotShow deriving Show

data OK = OK

instance Show OK where
    show _ = "OK"

data ThisWorks = ThisWorks OK deriving Show
