module Util.Format exposing
    ( float
    , int
    , padZero
    )

import FormatNumber
import FormatNumber.Locales


{-| Pad a string with a number of zeroes on the left side.
-}
padZero : Int -> String -> String
padZero n =
    String.padLeft n '0'


{-| Format an integer with thousands separator.

TODO: Handle different locales, e.g. "," for English and " " for Norwegian.

-}
int : Int -> Int -> String
int num =
    float (toFloat num)


{-| Format a float with thousands separator and the given number of decimals.

TODO: Handle different locales, e.g. "." for English, and "," for Norwegian.

-}
float : Float -> Int -> String
float num dec =
    let
        locale =
            FormatNumber.Locales.spanishLocale
    in
        FormatNumber.format { locale | decimals = dec } num
