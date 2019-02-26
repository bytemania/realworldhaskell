{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module WriterIO where

import SafeHello

import System.IO (IOMode(..))
import Control.Monad.Writer
import Control.Monad

data Event = Open FilePath IOMode | Put String String | Close String | GetContents String deriving Show

newtype WriterIO a = W {runW :: Writer [Event] a} deriving (Show, Functor, Applicative, Monad, MonadWriter [Event])

runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW


