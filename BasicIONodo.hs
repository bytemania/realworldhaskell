module BasicIONodo where

main :: IO ()
main = putStrLn "Greetings! What id your name?" >>
       getLine >>=
       (\inpStr -> putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!")



