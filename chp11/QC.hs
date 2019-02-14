module QC where

import Control.Monad (liftM, liftM2)
import Test.QuickCheck
import System.Random
import Data.Monoid
import Data.List (intersperse)

import Prettify2 (Doc(..))

instance Arbitrary Doc where
    arbitrary = do
        oneof [ return Empty
              , liftM Char arbitrary
              , liftM Text arbitrary
              , return Line
              , liftM2 Concat arbitrary arbitrary
              , liftM2 Union arbitrary arbitrary]

instance Semigroup Doc where
   f <> g = Union f g

instance Monoid Doc where
    mempty = empty

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text t = Text t

double :: Double -> Doc
double = text . show

prop_empty_id x = mempty <> x == x && x <> mempty == x

prop_char c = char c == Char c

prop_text s = text s == if null s then Empty else Text s

prop_double d = double d == text (show d)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

hcat = glue

prop_h_cat xs = hcat xs == glue xs

glue [] = empty
glue (d:ds) = d <> glue ds

punctuate s xs = intersperse s xs

prop_punctuate s xs = punctuate s xs == intersperse s xs

prop_punctuate' s xs = punctuate s xs == combine (intersperse s xs)
    where
        combine [] = []
        combine [x] = [x]

        combine (x:Empty:ys) = x : combine ys
        combine (Empty:y:ys) = y : combine ys
        combine (x:y:ys)     = x `Concat` y : combine ys
