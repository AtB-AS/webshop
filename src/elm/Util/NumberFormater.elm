module Util.NumberFormater exposing (Part(..), Pattern, formatString)

{-| Based on PhoneNumber: <https://github.com/dasch/elm-phone-number/blob/master/PhoneNumber.elm>
Apache 2.0 - by Daniel Schierbeck
-}

import Char
import String


type Part
    = Digits Int
    | Str String
    | Space


type alias Pattern =
    List Part


{-| Formats a string of digits according to the country code.
-}
formatString : Pattern -> String -> String
formatString pattern input =
    String.filter Char.isDigit input
        |> formatParts pattern
        |> String.concat



-- Internals


formatParts : Pattern -> String -> List String
formatParts parts chars =
    case parts of
        [] ->
            []

        part :: restParts ->
            let
                ( output, restChars ) =
                    formatPart part chars
            in
                output :: formatParts restParts restChars


formatPart : Part -> String -> ( String, String )
formatPart part digits =
    case part of
        Space ->
            ( " ", digits )

        Str string ->
            ( string, digits )

        Digits length ->
            ( String.left length digits
                |> String.padRight length placeholder
            , String.dropLeft length digits
            )


placeholder : Char
placeholder =
    ' '
