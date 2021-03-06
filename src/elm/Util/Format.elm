module Util.Format exposing
    ( date
    , dateTime
    , float
    , int
    , padZero
    , time
    )

import Data.FareContract exposing (FareTime)
import FormatNumber
import FormatNumber.Locales


{-| Pad a string with a number of zeroes on the left side.
-}
padZero : Int -> String -> String
padZero n =
    String.padLeft n '0'


{-| Format a FareTime as 12.03.2021
-}
date : FareTime -> String
date ft =
    String.join ""
        [ padZero 2 <| String.fromInt ft.day
        , "."
        , padZero 2 <| String.fromInt ft.month
        , "."
        , padZero 4 <| String.fromInt ft.year
        ]


{-| Format a FareTime as 09:41
-}
time : FareTime -> String
time ft =
    String.join ""
        [ padZero 2 <| String.fromInt ft.hour
        , ":"
        , padZero 2 <| String.fromInt ft.minute
        ]


{-| Format a FareTime as 12.03.2021 - 09:41
-}
dateTime : FareTime -> String
dateTime ft =
    date ft ++ " - " ++ time ft


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
