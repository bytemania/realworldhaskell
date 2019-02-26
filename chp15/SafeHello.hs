module SafeHello where

import MonadHandle
import MonadHandleIO
import System.IO (IOMode(..))
import System.Directory(removeFile)
import Control.Monad.Trans (MonadIO(..), MonadTrans(..))

safeHello :: MonadHandle h m => FilePath -> m ()
safeHello path = do
    h <- openFile path WriteMode
    hPutStrLn h "hello world"
    hClose h

tidierHello :: (MonadHandleIO h m) => FilePath -> m ()
tidierHello path = do
    safeHello path
    liftIO (removeFile path)

tidyHello :: (MonadIO m, MonadHandle h m) => FilePath -> m ()
tidyHello path = do
    safeHello path
    liftIO (removeFile path)

