{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module ParsecPlus where

import Control.Monad
import Text.ParserCombinators.Parsec

instance MonadPlus (GenParser tok st) where
    mzero = fail "mzero"
    mplus = (<|>)
