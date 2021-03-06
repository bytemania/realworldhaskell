module BetterPredicate where

import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import System.FilePath (takeExtension)
import Data.Time.Clock
import System.FilePath (takeExtension)
import Control.Exception (SomeException, handle, bracket)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

import RecursiveContents (getRecursiveContents)

type Predicate = FilePath -> Permissions -> Maybe Integer -> UTCTime -> Bool

betterFind p path = getRecursiveContents path >>= filterM check
    where check name = do
            perms <- getPermissions name
            size <- getFileSize name
            modified <- getModificationTime name
            return (p name perms size modified)

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize = saferFileSize

simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return size

saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = handle errorHandler calcSize
    where errorHandler :: SomeException -> IO (Maybe Integer)
          errorHandler _ = return Nothing

          calcSize :: IO (Maybe Integer)
          calcSize = bracket (openFile path ReadMode) hClose $ \h -> do
            size <- hFileSize h
            return (Just size)

myTest :: (Ord a, Num a) => FilePath -> p1 -> Maybe a -> p2 -> Bool
myTest path _ (Just size) _ = takeExtension path == ".cpp" && size > 131072
myTest _ _ _ _ = False

type InfoP a = FilePath -> Permissions -> Maybe Integer -> UTCTime -> a

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _     = -1

equalP :: Eq a => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k

equalP' :: Eq a => InfoP a -> a -> InfoP Bool
equalP' f k w x y z = f w x y z == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP :: Ord a => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)

lesserP :: Ord a => InfoP a -> a -> InfoP Bool
lesserP = liftP (<)

simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
simpleAndP f g w x y z = f w x y z && g w x y z

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP = liftP2 (&&)
orP = liftP2 (||)

constP :: a -> InfoP a
constP k _ _ _ _ = k

liftP' q f k w x y z = f w x y z `q` constP k w x y z

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

myTest2 :: InfoP Bool
myTest2 = (liftPath takeExtension `equalP` ".cpp") `andP` (sizeP `greaterP` 131072)

infix 4 ==?
(==?) :: Eq a => InfoP a -> a -> InfoP Bool
(==?) = equalP

infixr 3 &&?
(&&?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
(&&?) = andP

infix 4 >?
(>?) :: InfoP Integer -> Integer -> InfoP Bool
(>?) = greaterP

myTest3 :: InfoP Bool
myTest3 = (liftPath takeExtension ==? ".cpp") &&? (sizeP >? 131072)

myTest4 :: InfoP Bool
myTest4 = liftPath takeExtension ==? ".cpp" &&? sizeP >? 131072
