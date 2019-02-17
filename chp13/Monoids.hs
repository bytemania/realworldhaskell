module Monoids where

newtype AInt = A {unA :: Int} deriving (Show, Eq)

instance Semigroup AInt where
    (A x) <> (A y) = A (x + y)

instance Monoid AInt where
    mempty = A 0

newtype MInt = M {unM :: Int} deriving (Show, Eq)

instance Semigroup MInt where
    (M x) <> (M y) = M (x * y)

instance Monoid MInt where
    mempty = M 1

