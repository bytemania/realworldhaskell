module FunctionalProgramming where

import Data.Char (digitToInt, isDigit, toUpper)
import Control.Applicative
import Data.List

splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
    let isLineTerminator c = c == '\r' || c == '\n'
        (pre, suf) = break isLineTerminator cs
    in pre : case suf of
            ('\r':'\n':rest) -> splitLines rest
            ('\r': rest)     -> splitLines rest
            ('\n': rest)     -> splitLines rest
            _                -> []

fixLines :: String -> String
fixLines input = unlines (splitLines input)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (h:_) = Just h

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:t) = Just t

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (h:[]) = Just h
safeLast (_:t) = safeLast t

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit (h:[]) = Just []
safeInit (h:t) = (:) <$> Just h <*> safeInit t

asInt :: String -> Int
asInt = loop 0
    where loop acc [] = acc
          loop acc (h:t) = let acc' = acc * 10 + digitToInt h in loop acc' t

square :: [Double] -> [Double]
square [] = []
square (h:t) = h * h : square t

upperCase :: String -> String
upperCase [] = []
upperCase (h:t) = toUpper h : upperCase t

square2 :: Num a => [a] -> [a]
square2 xs = map (\x -> x * x) xs

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (h:t) = f h : myMap f t

identity :: [a] -> [a]
identity xs = foldr (:) [] xs

append :: [a] -> [a] -> [a]
append la lb = foldr (:) lb la

asInt_fold :: String -> Int
asInt_fold = foldl (\acc x-> acc * 10 + digitToInt x) 0

type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either = loop 0
    where loop acc [] = Right acc
          loop acc (h:t) = if isDigit h then let acc' = acc * 10 + digitToInt h in loop acc' t
                           else Left "Not a valid Number"

concat' :: [[a]] -> [a]
concat' = foldr (++) []

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (h:t)
    | f h = h: takeWhile' f t
    | otherwise = []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' f = foldr (\x acc -> if f x then x:acc else acc ) []

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' pred input = foldl foldFunc [] input
    where foldFunc [] elem = [[elem]]
          foldFunc acc elem
            | pred (head(last acc)) elem = (init acc) ++ [(last acc ++ [elem])]
            | otherwise = acc ++ [[elem]]

any' :: (a -> Bool) -> [a] -> Bool
any' f = foldl (\acc x -> (acc || f x)) False

words' :: String -> [String]
words' s = foldl f [] (reverse.dropWhile (== ' ').reverse $ s)
    where f [] ' ' = []
          f acc ' ' = acc ++ [[]]
          f [] x = [[x]]
          f acc x = init acc ++ [last acc ++ [x]]

unlines' :: [String] -> String
unlines' = foldl (\x acc -> x ++ "\n" ++acc) []


