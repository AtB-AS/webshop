module Format exposing (date, datetime, float, int, time)

import Time exposing (Month(..), Posix, Zone)


int : Int -> String
int num =
    String.join "," <| splitThousands num


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


splitThousands : Int -> List String
splitThousands num =
    if num >= 1000 then
        [ modBy 1000 num ]
            |> List.map String.fromInt
            |> List.map (String.padLeft 3 '0')
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
        |> String.padLeft digits '0'


{-| Format a `Posix` date/time according to YYYY-MM-DD HH:MM:SS.
-}
datetime : Zone -> Posix -> String
datetime zone posix =
    date zone posix ++ " " ++ time zone posix


{-| Format a `Posix` date according to YYYY-MM-DD.
-}
date : Zone -> Posix -> String
date zone posix =
    let
        year =
            Time.toYear zone posix

        month =
            case Time.toMonth zone posix of
                Jan ->
                    1

                Feb ->
                    2

                Mar ->
                    3

                Apr ->
                    4

                May ->
                    5

                Jun ->
                    6

                Jul ->
                    7

                Aug ->
                    8

                Sep ->
                    9

                Oct ->
                    10

                Nov ->
                    11

                Dec ->
                    12

        day =
            Time.toDay zone posix
    in
        String.join "-" [ pad4 year, pad2 month, pad2 day ]


{-| Format a `Posix` time according to HH:MM:SS.
-}
time : Zone -> Posix -> String
time zone posix =
    let
        hour =
            Time.toHour zone posix

        min =
            Time.toMinute zone posix

        sec =
            Time.toSecond zone posix
    in
        String.join ":" [ pad2 hour, pad2 min, pad2 sec ]



-- INTERNAL


pad2 : Int -> String
pad2 =
    String.fromInt >> String.padLeft 2 '0'


pad4 : Int -> String
pad4 =
    String.fromInt >> String.padLeft 4 '0'
