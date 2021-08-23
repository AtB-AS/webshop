module Data.Ticket exposing
    ( Offer
    , PaymentStatus
    , Price
    , RecurringPayment
    , Reservation
    , ReservationStatus(..)
    , Ticket
    )

import Data.PaymentType exposing (PaymentType)
import Data.RefData exposing (UserType)


type alias Ticket =
    { duration : Int
    , orderId : String
    , orderVersion : String
    , productName : String
    , usageValidFrom : Int
    , usageValidTo : Int
    , userProfiles : List String
    }


type alias Price =
    { amount : String
    , amountFloat : Float
    , currency : String
    }


type alias Offer =
    { offerId : String
    , userType : UserType
    , prices : List Price
    , travellerId : String
    , validTo : String
    , validFrom : String
    }


type ReservationStatus
    = Captured
    | NotCaptured


type alias Reservation =
    { orderId : String
    , paymentId : Int
    , transactionId : Int
    , url : String
    }


type alias PaymentStatus =
    { orderId : String
    , status : String
    , paymentType : String
    }


type alias RecurringPayment =
    { id : Int
    , paymentType : PaymentType
    , maskedPan : String
    , expiresAt : String
    }
