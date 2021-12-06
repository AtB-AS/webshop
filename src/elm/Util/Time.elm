module Util.Time exposing (addHours, isoStringToCardExpirationMonth, isoStringToFullHumanized, millisToDateHumanized, millisToFullHumanized, toDateHumanized, toFullHumanized, toHoursAndMinutes, toIsoDate, toIsoTime, toMonthNum)

import DateFormat
import Iso8601
import Time exposing (Posix)
import Util.Format as Format


{-| Convert a timestamp to an ISO-formatted string such as "2021-02-03".
-}
toIsoDate : Time.Zone -> Time.Posix -> String
toIsoDate zone time =
    let
        parts =
            [ Time.toYear zone time
            , toMonthNum (Time.toMonth zone time)
            , Time.toDay zone time
            ]
    in
        parts
            |> List.map String.fromInt
            |> List.map (Format.padZero 2)
            |> String.join "-"


{-| Convert a timestamp to an ISO-formatted time string such as "17:12:34".
-}
toIsoTime : Time.Zone -> Time.Posix -> String
toIsoTime zone time =
    let
        parts =
            [ Time.toHour zone time
            , Time.toMinute zone time
            , Time.toSecond zone time
            ]
    in
        parts
            |> List.map String.fromInt
            |> List.map (Format.padZero 2)
            |> String.join ":"


toMonthNum : Time.Month -> Int
toMonthNum month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


millisToDateHumanized : Time.Zone -> Int -> String
millisToDateHumanized zone ms =
    ms |> Time.millisToPosix |> toDateHumanized zone


toDateHumanized : Time.Zone -> Time.Posix -> String
toDateHumanized zone date_ =
    toFormat zone date_ "dd.MM.yyyy"


isoStringToFullHumanized : Time.Zone -> String -> Maybe String
isoStringToFullHumanized zone dateString =
    case Iso8601.toTime dateString of
        Err _ ->
            Nothing

        Ok timePosix ->
            Just <| toFullHumanized zone timePosix


millisToFullHumanized : Time.Zone -> Int -> String
millisToFullHumanized zone ms =
    ms |> Time.millisToPosix |> toFullHumanized zone


toFullHumanized : Time.Zone -> Time.Posix -> String
toFullHumanized zone date_ =
    toFormat zone date_ "dd.MM.yyyy - HH:mm"


{-| Convert a ISO-formatted datetime string to a card expiration month.
A ISO string of 2021-10-01T00:00:00Z should be represented as 09/21.
-}
isoStringToCardExpirationMonth : Time.Zone -> String -> Maybe String
isoStringToCardExpirationMonth zone dateString =
    case Iso8601.toTime dateString of
        Err _ ->
            Nothing

        Ok timePosix ->
            Just <| toFormat zone (addHours -24 timePosix) "MM/yy"


toHoursAndMinutes : Time.Zone -> Time.Posix -> String
toHoursAndMinutes zone date_ =
    toFormat zone date_ "HH:mm"


{-| Add hours to specific posix object
-}
addHours : Int -> Posix -> Posix
addHours hours posix =
    posix
        |> Time.posixToMillis
        |> (+) (hours * 1000 * 60 * 60)
        |> Time.millisToPosix



-- INTERNAL


toFormat : Time.Zone -> Time.Posix -> String -> String
toFormat zone date_ format =
    DateFormat.formatI18n DateFormat.norwegian format zone date_
