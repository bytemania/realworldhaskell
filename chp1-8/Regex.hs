module Regex where

import qualified Data.ByteString.Lazy as L

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
    where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO Bool
isElfFile path = do
    content <- L.readFile path
    return (hasElfMagic content)

closing = readPrice . (!!4) . L.split ','

readPrice :: L.ByteString -> Maybe Int
readPrice str =
    case readInt str of
        Nothing              -> Nothing
        Just (dollars, rest) ->
            case readInt (L.tail rest) of
                Nothing            -> Nothing
                Just (cents, more) -> Just (dollars * 100 + cents)

highestClose = maximum . (Nothing:) . map closing . lines

highestCloseFrom path = do
    contents <- L.readFile path
    print (highestClose contents)


