module SupplyInstance where

newtype Reader e a = R {runReader :: e -> a}

instance Functor (Reader e) where
    fmap f m = R $ \r -> f (runReader m r)

instance Applicative (Reader e) where
    pure = return

instance Monad (Reader e) where
    return a = R $ \_ -> a
    m >>= k = R $ \r -> runReader (k (runReader m r)) r

ask :: Reader e e
ask = R id

newtype MySupply e a = MySupply {runMySupply :: Reader e a} -- deriving (Monad)


-- instance MonadSupply e (MySupply e) where
--    next = MySupply $ do
--             v <- ask
--             return (Just v)


--xy :: (Num s, MonadSupply s m) => m s
--xy = do
--        Just x <- next
--        Just y <- next
--        return (x * y)



