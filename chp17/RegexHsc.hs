{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module RegexHsc where


import Foreign
import Foreign.C.Types

#include <pcre.h>

newtype PCREOption = PCREOption { unPCREOption :: CInt } deriving (Eq,Show)

