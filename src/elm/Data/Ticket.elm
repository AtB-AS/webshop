module Data.Ticket exposing (Offer, Price, Reservation, Ticket)


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
    , prices : List Price
    , travellerId : String
    }


type alias Reservation =
    { orderId : String
    , paymentId : Int
    , transactionId : Int
    , url : String
    }
