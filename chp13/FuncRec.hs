module FuncRec where

data FuncRec = FuncRec {name :: String, calc :: Int -> Int, namedCalc :: Int -> (String, Int)}

mkFuncRec :: String -> (Int -> Int) -> FuncRec
mkFuncRec name calcfunc = FuncRec {name = name, calc = calcfunc, namedCalc = \x -> (name, calcfunc x)}

plus5 :: FuncRec
plus5 = mkFuncRec "plus5" (+5)

always0 :: FuncRec
always0 = mkFuncRec "always 0" (\_ -> 0)

