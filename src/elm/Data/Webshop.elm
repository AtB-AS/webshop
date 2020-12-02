module Data.Webshop exposing
    ( FareContract
    , FareContractState(..)
    , Profile
    , Token
    , TokenAction(..)
    , TokenStatus(..)
    , TokenType(..)
    )


type alias Profile =
    { email : String
    , firstName : String
    , lastName : String
    , customerNumber : Int
    }


type TokenType
    = TokenTypeUnspecified
    | TokenTypeQrSmartphone
    | TokenTypeQrPaper
    | TokenTypeTravelCard
    | TokenTypeReferenceCode
    | TokenTypePlainUnsigned
    | TokenTypeExternal


type TokenAction
    = TokenActionUnspecified
    | TokenActionTicketTransfer
    | TokenActionAddRemoveToken
    | TokenActionIdentification
    | TokenActionTicketInspection
    | TokenActionGetFareContracts
    | TokenActionTravelCard


type TokenStatus
    = TokenStatusUnspecified
    | TokenStatusActive
    | TokenStatusInactive
    | TokenStatusOther


type alias Token =
    { id : String
    , allowedAction : List TokenAction
    , status : TokenStatus
    , type_ : TokenType
    }


type FareContractState
    = FareContractStateUnspecified
    | FareContractStateNotActivated
    | FareContractStateActivated
    | FareContractStateCancelled
    | FareContractStateRefunded


type alias FareContract =
    { id : String
    , state : FareContractState
    }
