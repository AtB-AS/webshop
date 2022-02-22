module Data.Reservation exposing (PaymentStatus(..), Reservation)

import Data.PaymentType exposing (PaymentType)


type alias Reservation =
    { created : Int
    , orderId : String
    , paymentId : Int
    , transactionId : Int
    , url : String
    , paymentStatus : Maybe PaymentStatus
    , paymentType : Maybe PaymentType
    }


type PaymentStatus
    = AUTHENTICATE
    | CANCEL
    | CAPTURE
    | CREATE
    | CREDIT
    | IMPORT
    | INITIATE
    | REJECT
