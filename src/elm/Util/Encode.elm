module Util.Encode exposing (maybe)

import Json.Encode as Encode exposing (Value)


maybe : Maybe a -> (a -> Value) -> Value
maybe maybeValue encoder =
    case maybeValue of
        Just value ->
            encoder value

        Nothing ->
            Encode.null
