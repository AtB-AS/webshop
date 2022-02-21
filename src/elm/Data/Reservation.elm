module Data.Reservation exposing (..)

import Data.PaymentType exposing (PaymentType)


type alias Reservation =
    { created : Int
    , orderId : String
    , paymentId : Int
    , transactionId : Int
    , url : String
    , paymentType : PaymentType
    , paymentStatus : ReservationStatus
    }


type ReservationStatus
    = Captured
    | NotCaptured
