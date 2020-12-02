module Data.Webshop exposing
    ( FareContract
    , FareContractState(..)
    , InspectionResult(..)
    , Profile
    , RejectionReason(..)
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


type RejectionReason
    = RejectionReasonNoActiveFareContracts -- 1
    | RejectionReasonNoFareContracts -- 2
    | RejectionReasonFareContractNotActivated -- 3
    | RejectionReasonValidityParametersInvalid -- 4
    | RejectionReasonTokenMarkedInactive -- 100
    | RejectionReasonTokenValidityNotStarted -- 101
    | RejectionReasonTokenValidityEnded -- 102
    | RejectionReasonTokenSignatureInvalid -- 103
    | RejectionReasonTokenNotFound -- 104
    | RejectionReasonDifferentTokenType -- 105
    | RejectionReasonTokenIdMismatch -- 106
    | RejectionReasonTokenActionsMismatch -- 107


type InspectionResult
    = InspectionGreen
    | InspectionYellow
    | InspectionRed RejectionReason
