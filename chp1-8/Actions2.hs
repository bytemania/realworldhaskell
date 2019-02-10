module Actions2 where

str2action :: String -> IO ()
str2action input = putStrLn ("Data: " ++ input)

list2actions :: [String] -> [IO ()]
list2actions = map str2action

numbers :: [Int]
numbers = [1..10]

main = do
    str2action "Start of the program"
    mapM_ (str2action . show) numbers
    str2action "Done!"

