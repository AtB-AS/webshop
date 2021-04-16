module Util.Format exposing
    ( date
    , dateTime
    , float
    , int
    , isoStringToFullHumanized
    , padZero
    , time
    )

import Data.FareContract exposing (FareTime)
import DateFormat
import FormatNumber
import FormatNumber.Locales exposing (Locale)
import Iso8601
import Time exposing (Posix)


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



-- INTERNAL


splitThousands : Int -> List String
splitThousands num =
    if num >= 1000 then
        [ modBy 1000 num ]
            |> List.map String.fromInt
            |> List.map (padZero 3)
            |> List.append (splitThousands <| num // 1000)

    else
        [ String.fromInt num ]


decimals : Int -> Float -> String
decimals digits num =
    digits
        |> toFloat
        |> (^) 10
        |> (*) num
        |> round
        |> splitThousands
        |> String.concat
        |> String.right digits
        |> padZero digits


isoStringToFullHumanized : Time.Zone -> String -> Maybe String
isoStringToFullHumanized zone dateString =
    case Iso8601.toTime dateString of
        Err deadEnds ->
            Nothing

        Ok timePosix ->
            Just <| DateFormat.formatI18n DateFormat.norwegian "dd.MM.yyyy, HH:mm" zone timePosix
