module SimpleFinder where

import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import Data.Time.Clock.System
import System.FilePath (takeExtension)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import Control.Exception (bracket, handle)

import RecursiveContents (getRecursiveContents)

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = do
    names <- getRecursiveContents path
    return (filter p names)