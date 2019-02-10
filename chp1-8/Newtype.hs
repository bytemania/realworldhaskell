module Newtype where

import Control.Arrow (second)

data DataInt = D Int deriving (Eq, Ord, Show)

newtype NewtypeInt = N Int deriving (Eq, Ord, Show)

newtype UniqueID = UniqueId Int deriving Eq

data TwoFields = TwoFields Int Int

newtype Okay = ExactlyOne Int

newtype Param a b = Param (Either a b)

newtype Record = Record {getInt :: Int}

newtype JAry a = JAry {fromJAry :: [a]} deriving (Eq, Ord, Show)

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a) = Right (f a)

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
                        Left err -> Left err
                        Right ys -> case f x of
                            Left err -> Left err
                            Right y -> Right (y:ys)
mapEithers _ _ = Right []

instance (JSON a) => JSON (JObj a) where
    toJValue = JObject . JObj . map (second toJValue) . fromJObj
    fromJValue (JObject (JObj o)) = whenRight JObj (mapEithers unwrap o)
        where unwrap (k,v) = whenRight ((,) k) (fromJValue v)
    fromJValue _ = Left "not a JSON object"


