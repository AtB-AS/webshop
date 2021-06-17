module Data.Webshop exposing
    ( FareContract
    , FareContractState(..)
    , GivenConsent
    , Inspection(..)
    , Profile
    , Rejection(..)
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
    | TokenTypeQrPaper String
    | TokenTypeTravelCard String
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
    , validity : ( Int, Int )
    }


type FareContractState
    = FareContractStateUnspecified
    | FareContractStateNotActivated
    | FareContractStateActivated
    | FareContractStateCancelled
    | FareContractStateRefunded


type alias FareContract =
    { id : String
    , validity : ( Int, Int )
    , state : FareContractState
    , userProfiles : List String
    , fareProducts : List String
    }


type Rejection
    = RejectionNoActiveFareContracts
    | RejectionNoFareContracts
    | RejectionFareContractNotActivated
    | RejectionValidityParametersInvalid
    | RejectionTokenMarkedInactive
    | RejectionTokenValidityNotStarted
    | RejectionTokenValidityEnded
    | RejectionTokenSignatureInvalid
    | RejectionTokenNotFound
    | RejectionDifferentTokenType
    | RejectionTokenIdMismatch
    | RejectionTokenActionsMismatch


type Inspection
    = InspectionGreen
    | InspectionYellow
    | InspectionRed Rejection


type alias GivenConsent =
    { id : Int
    , consentId : Int
    , choice : Bool
    , email : String
    }
