module Data.PaymentType exposing
    ( PaymentCard(..)
    , PaymentSelection(..)
    , PaymentType(..)
    , format
    , fromEntur
    , fromInt
    , fromString
    , toIcon
    , toInt
    , toString
    )

import Fragment.Icon as Icon
import Html exposing (Html)
import Html.Attributes as A


type PaymentCard
    = Visa
    | MasterCard
    | AmericanExpress


type PaymentType
    = Nets PaymentCard
    | Vipps


type PaymentSelection
    = NonRecurring PaymentType
    | Recurring Int


{-| Parse payment type ids coming from Entur's API.
-}
fromEntur : String -> Maybe PaymentType
fromEntur paymentType =
    case paymentType of
        "VISA" ->
            Just <| Nets Visa

        "MASTERCARD" ->
            Just <| Nets MasterCard

        "AMEX" ->
            Just <| Nets AmericanExpress

        "VIPPS" ->
            Just Vipps

        _ ->
            Nothing


fromString : String -> Maybe PaymentType
fromString paymentType =
    case paymentType of
        "visa" ->
            Just <| Nets Visa

        "mastercard" ->
            Just <| Nets MasterCard

        "amex" ->
            Just <| Nets AmericanExpress

        "vipps" ->
            Just Vipps

        _ ->
            Nothing


toString : PaymentType -> String
toString paymentType =
    case paymentType of
        Nets Visa ->
            "visa"

        Nets MasterCard ->
            "mastercard"

        Nets AmericanExpress ->
            "amex"

        Vipps ->
            "vipps"


format : PaymentType -> String
format paymentType =
    case paymentType of
        Nets Visa ->
            "Visa"

        Nets MasterCard ->
            "MasterCard"

        Nets AmericanExpress ->
            "American Express"

        Vipps ->
            "Vipps"


toIcon : PaymentType -> Html msg
toIcon paymentType =
    case paymentType of
        Nets Visa ->
            Html.img [ A.src "images/paymentcard-visa.svg" ] []

        Nets MasterCard ->
            Html.img [ A.src "images/paymentcard-mastercard.svg" ] []

        _ ->
            Icon.creditcard


toInt : PaymentType -> Int
toInt paymentType =
    case paymentType of
        Nets Visa ->
            3

        Nets MasterCard ->
            4

        Nets AmericanExpress ->
            5

        Vipps ->
            2


fromInt : Int -> Maybe PaymentType
fromInt paymentType =
    case paymentType of
        3 ->
            Just <| Nets Visa

        4 ->
            Just <| Nets MasterCard

        5 ->
            Just <| Nets AmericanExpress

        2 ->
            Just Vipps

        _ ->
            Nothing
