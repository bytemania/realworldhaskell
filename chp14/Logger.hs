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

globToRegex :: String -> Logger String
globToRegex cs = globToRegex' cs >>= \ds -> return ('^':ds)

globToRegex' :: String -> Logger String
globToRegex' "" = return "$"
globToRegex' ('?' : cs) = record "any" >> globToRegex' cs >>=  \ds -> return ('.':ds)
globToRegex' ('*':cs) = do
    record "kleene star"
    ds <- globToRegex' cs
    return (".*" ++ ds)
globToRegex' ('[':'!':c:cs) =
    record "character class, negative" >>
    charClass cs >>= \ds ->
    return ("[^" ++ c : ds)
globToRegex' ('[':c:cs) =
    record "character class" >>
    charClass cs >>= \ds ->
    return ("[" ++ c : ds)
globToRegex' ('[':_) =
    fail "unterminated character class"

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = m >>= (\i -> return (f i))

charClass_wordy (']':cs) = globToRegex' cs >>= \ds -> return (']':ds)
charClass_wordy (c:cs) = charClass_wordy cs >>= \ds -> return (c:ds)

charClass (']':cs) = (']':) `liftM` globToRegex' cs
charClass (c:cs) = (c:) `liftM` charClass cs

globToRegex'' (c:cs) = liftM2 (++) (escape c) (globToRegex' cs)

escape :: Char -> Logger String
escape c
    | c `elem` regexChars = record "escape" >> return ['\\',c]
    | otherwise           = return [c]
    where regexChars = "\\+()^$.{}]|"

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m1 m2 = do
    a <- m1
    b <- m2
    return (f a b)


