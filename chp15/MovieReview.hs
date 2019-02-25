module MovieReview where

movie :: [(String, Maybe String)]
movie = [("name", Just "Attila \"The Hun\""), ("occupation", Just "Khan")]

data MovieReview = MovieReview {
      revTitle :: String
    , revUser :: String
    , revReview :: String
}

simpleReview :: [(String, Maybe String)] -> Maybe MovieReview
simpleReview alist =
    case lookup "title" alist of
        Just (Just title@(_:_)) ->
            case lookup "user" alist of
                Just (Just user@(_:_)) ->
                    case lookup "review" alist of
                        Just (Just review@(_:_)) ->
                            Just (MovieReview title user review)
                        _ -> Nothing
                _ -> Nothing
        _ -> Nothing

maybeReview :: [(String, Maybe String)] -> Maybe MovieReview
maybeReview alist = do
    title <- lookup "title" alist
    user <- lookup "user" alist
    review <- lookup "review" alist
    return (MovieReview title user review)

liftedReview :: [(String, Maybe String)] -> Maybe MovieReview
liftedReview alist = liftM3 MovieReview (lookup "title" alist) (lookup "user" alist) (lookup "review" alist)



