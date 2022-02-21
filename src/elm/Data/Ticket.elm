module Data.Ticket exposing
    ( Offer
    , Price
    , RecurringPayment
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


type alias RecurringPayment =
    { id : Int
    , paymentType : PaymentType
    , maskedPan : String
    , expiresAt : String
    }
