module Data.PaymentTypeGroup exposing
    ( PaymentTypeGroup(..)
    , format
    , fromEntur
    )


type PaymentTypeGroup
    = PaymentCard
    | Mobile
    | Cash
    | Ecard
    | Agent
    | Remitted
    | GiftCard
    | Requisition
    | Invoice


{-| Parse payment type group coming from Entur's API.
-}
fromEntur : String -> Maybe PaymentTypeGroup
fromEntur paymentTypeGroup =
    case paymentTypeGroup of
        "PAYMENTCARD" ->
            Just PaymentCard

        "MOBILE" ->
            Just Mobile

        "CASH" ->
            Just Cash

        "ECARD" ->
            Just Ecard

        "AGENT" ->
            Just Agent

        "REMITTED" ->
            Just Remitted

        "GIFTCARD" ->
            Just GiftCard

        "REQUISITION" ->
            Just Requisition

        "INVOICE" ->
            Just Invoice

        _ ->
            Nothing


format : PaymentTypeGroup -> String
format paymentTypeGroup =
    case paymentTypeGroup of
        PaymentCard ->
            "Betalingskort"

        Mobile ->
            "App"

        Cash ->
            "Kontanter"

        Ecard ->
            "E-kort"

        Agent ->
            "Agent"

        Remitted ->
            "Ettergitt"

        GiftCard ->
            "Gavekort"

        Requisition ->
            "Rekvisisjon"

        Invoice ->
            "Faktura"
