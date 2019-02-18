module Maybe where

data Maybe' a = Nothing' | Just' a deriving (Show, Eq)

instance Functor Maybe' where
    fmap _ Nothing' = Nothing'
    fmap f (Just' a) = Just' (f a)

instance Applicative Maybe' where
    pure = Just'
    Nothing' <*> _ = Nothing'
    Just' f  <*> something = fmap f something

instance Monad Maybe' where
    return = Just'
    Nothing' >>= _ = Nothing'
    Just' a  >>= f = f a

    Just' _  >> k   =  k
    Nothing' >> _  =  Nothing'

    fail _ = Nothing'

maybe' :: b -> (a -> b) -> Maybe' a -> b
maybe' n _ Nothing' = n
mabye' _ f (Just' a) = f a

