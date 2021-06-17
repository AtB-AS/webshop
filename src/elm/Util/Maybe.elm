module Util.Maybe exposing (flatMap, join, mapWithDefault)

{-| Utils for operating on Maybes, adding more combinators
to treat them as functors.
-}


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


{-| Helper for mapping a Maybe and then providing a default value.
-}
mapWithDefault : (a -> b) -> b -> Maybe a -> b
mapWithDefault f default maybe =
    maybe
        |> Maybe.map f
        |> Maybe.withDefault default
