module Util.Format exposing
    ( date
    , dateTime
    , float
    , int
    , padZero
    , time
    )

import Data.FareContract exposing (FareTime)


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
int : Int -> String
int num =
    String.join " " <| splitThousands num


{-| Format a float with thousands separator and the given number of decimals.

TODO: Handle different locales, e.g. "." for English, and "," for Norwegian.

-}
float : Float -> Int -> String
float num dec =
    let
        truncated =
            truncate num

        integer =
            if truncated < 0 then
                -truncated

            else
                truncated
    in
        String.concat [ int integer, ".", decimals dec num ]



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
