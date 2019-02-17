module BuildMap where

import qualified Data.Map as Map

al :: [(Int,String)]
al = [(1,"one"),(2,"two"),(3,"three"),(4,"four")]

mapFromAL :: Map.Map Int String
mapFromAL = Map.fromList al

mapFold :: Map.Map Int String
mapFold = foldl (\map (k,v) -> Map.insert k v map) Map.empty al

mapManual :: Map.Map Int String
mapManual = Map.insert 2 "two" . Map.insert 4 "four" . Map.insert 1 "one" . Map.insert 3 "three" $ Map.empty

data CustomColor = CustomColor {red :: Int, green :: Int, blue :: Int} deriving (Eq, Show, Read)

data FuncRec = FuncRec {name :: String, colorCalc :: Int -> (CustomColor, Int)}

plus5func :: CustomColor -> Int -> (CustomColor, Int)
plus5func color x = (color, x + 5)

purple :: CustomColor
purple = CustomColor 255 0 255

plus5 :: FuncRec
plus5 = FuncRec {name = "plus5", colorCalc = plus5func purple}

always0 :: FuncRec
always0 = FuncRec {name = "always0", colorCalc = \_ -> (purple, 0)}

