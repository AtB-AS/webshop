module Util.Maybe exposing (flatMap, join)

{-| flatMap combinator for Maybes
-}


flatMap : (a -> Maybe b) -> Maybe a -> Maybe b
flatMap f maybe =
    Maybe.map f maybe
        |> join


{-| flatten two maybes
-}
join : Maybe (Maybe a) -> Maybe a
join =
    Maybe.andThen identity
