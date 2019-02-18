module Logger (
      Logger
    , Log
    , runLogger
    , record
) where

newtype Logger a = Logger {execLogger :: (a, Log)}

type Log = [String]

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

record :: String -> Logger ()
record s = Logger ((), [s])

instance Functor Logger where
    fmap f (Logger (a, l)) = Logger (f a, l)

instance Applicative Logger where
    pure a = Logger (a, [])
    (Logger (f, lf)) <*> (Logger (a, la)) = Logger (f a, lf ++ la)

instance Monad Logger where
    return a = Logger (a, [])
    m >>= k = let (a,w) = execLogger m
                  n     = k a
                  (b,x) = execLogger n
             in Logger (b, w ++ x)

--globToRegex :: String -> Logger String

