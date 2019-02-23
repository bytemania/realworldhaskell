module State where

import System.Random


newtype State s a = State {
    runState :: s -> (a, s)
}

returnState :: a -> State s a
returnState a = State $ \s -> (a, s)

bindState :: State s a -> (a -> State s b) -> State s b
bindState m k = State $ \s -> let (a, s') = runState m s
                              in runState (k a) s'

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put s = State $ \s -> ((), s)


rand :: IO Int
rand = getStdRandom (randomR (0, maxBound))

--bindState' :: State s a -> (a -> State s b) -> State s b
bindState' k f = k `bindState` \_ -> f


