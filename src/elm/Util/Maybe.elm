module Util.Maybe exposing (filter, flatMap, join, mapWithDefault)

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


filter : (a -> Bool) -> Maybe a -> Maybe a
filter f maybe =
    Maybe.andThen
        (\m ->
            if f m then
                maybe

            else
                Nothing
        )
        maybe
