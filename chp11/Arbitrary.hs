module Arbitrary where

import Test.QuickCheck(Gen)
import System.Random

class Arbitrary a where
    arbitrary :: Gen a

elements :: [a] -> Gen a
elements = undefined

choose   :: Random a => (a, a) -> Gen a
choose = undefined

oneof    :: [Gen a] -> Gen a
oneof = undefined

data Ternary = Yes | No | Unknown deriving (Eq,Show)

instance Arbitrary Ternary where
    arbitrary = elements [Yes, No, Unknown]

instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (x, y)

instance Arbitrary Char where
    arbitrary = elements (['A'..'Z'] ++ ['a'..'z'] ++ " ~!@#$%^&*()")

