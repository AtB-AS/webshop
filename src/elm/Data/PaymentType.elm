module Data.PaymentType exposing
    ( PaymentCard(..)
    , PaymentType(..)
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
