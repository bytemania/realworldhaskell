module MultiplyTo where

guarded :: Bool -> [a] -> [a]
guarded True l = l
guarded False _ = []

multiplyTo :: Int -> [(Int, Int)]
multiplyTo n = do
    x <- [1..n]
    y <- [x..n]
    guarded (x * y == n) (return (x,y))

robust :: [a] -> Maybe a
robust xs = do (_:x:_) <- Just xs
               return x




