module Data.FareContract exposing
    ( FareContract
    , FareContractState(..)
    , TravelRight(..)
    , TravelRightBase
    , TravelRightCarnet
    , TravelRightFull
    , UsedAccess
    )

import Data.PaymentType exposing (PaymentType)
import Data.PaymentTypeGroup exposing (PaymentTypeGroup)


type TravelRight
    = SingleTicket TravelRightFull
    | PeriodTicket TravelRightFull
    | CarnetTicket TravelRightCarnet
    | UnknownTicket TravelRightBase


type alias TravelRightFull =
    { id : String
    , status : Int
    , fareProductRef : String
    , startDateTime : Int
    , endDateTime : Int
    , usageValidityPeriodRef : String
    , userProfileRef : String
    , authorityRef : String
    , tariffZoneRefs : List String
    }


type alias TravelRightCarnet =
    { id : String
    , status : Int
    , fareProductRef : String
    , startDateTime : Int
    , endDateTime : Int
    , usageValidityPeriodRef : String
    , userProfileRef : String
    , authorityRef : String
    , tariffZoneRefs : List String
    , maximumNumberOfAccesses : Int
    , numberOfUsedAccesses : Int
    , usedAccesses : List UsedAccess
    }


type alias UsedAccess =
    { startDateTime : Int
    , endDateTime : Int
    }


type alias TravelRightBase =
    { id : String
    , status : Maybe Int
    }


type alias FareContract =
    { created : Int
    , version : String
    , orderId : String
    , minimumSecurityLevel : Int
    , id : String
    , travelRights : List TravelRight
    , state : FareContractState
    , qrCode : Maybe String
    , validFrom : Int
    , validTo : Int
    , totalAmount : Maybe String
    , paymentType : List PaymentType
    , paymentTypeGroup : List PaymentTypeGroup
    }


type FareContractState
    = FareContractStateUnspecified
    | FareContractStateNotActivated
    | FareContractStateActivated
    | FareContractStateCancelled
    | FareContractStateRefunded
