module Data.PaymentType exposing
    ( PaymentCard(..)
    , PaymentType(..)
    , format
    , fromEntur
    , fromString
    , toInt
    , toString
    )


type PaymentCard
    = Visa
    | MasterCard
    | AmericanExpress


type PaymentType
    = Nets PaymentCard
    | Vipps


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
