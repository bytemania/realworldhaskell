module SumFile where

import qualified Data.ByteString.Lazy as L

main :: IO ()
main = do
    contents <- getContents
    print $ sumFile contents
        where sumFile = sum . map read . words


