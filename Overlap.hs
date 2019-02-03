{-# LANGUAGE OverlappingInstances #-}

module Overlap where

class Borked a where
    bork :: a -> String

instance Borked Int where
    bork = show

instance (Borked a, Borked b) => Borked (a, b) where
    bork (a, b) = ">>" ++ bork a ++ " " ++ bork b ++ "<<"

