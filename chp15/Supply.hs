module Supply (Supply, next, runSupply) where

import Control.Monad.State

runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (S m) xs = runState m xs

newtype Supply s a = S (State [s] a)

unwrapS :: Supply s a -> State [s] a
unwrapS (S s) = s

instance Functor (Supply s) where
    fmap f s = S $ state $ \st -> let (x, st') = runState (unwrapS s) st
                                  in  runState ((return . f) x) st'

instance Applicative (Supply s) where
    pure a = S $ do return  a

instance Monad (Supply s) where
    s >>= m = S (unwrapS s >>= unwrapS . m)
    return = S . return

next :: Supply s (Maybe s)
next = S $ do st <- get
              case st of
                [] -> return Nothing
                (x:xs) -> do put xs
                             return (Just x)

showTwo :: Show s => Supply s String
showTwo = do
    a <- next
    b <- next
    return (show "a: " ++ show a ++ ", b: " ++ show b)