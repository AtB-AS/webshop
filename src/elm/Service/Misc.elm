port module Service.Misc exposing
    ( Profile
    , SignInMethod
    , SignInProvider(..)
    , bodyClass
    , carnetTravelRightDecoder
    , closeValidityWarning
    , convertTime
    , convertedTime
    , fareContractDecoder
    , navigateTo
    , onProfileChange
    , onboardingDone
    , onboardingRefreshAuth
    , onboardingStart
    , receiveFareContracts
    , receiveTokens
    , reloadPage
    , saveProfile
    )

import Data.FareContract exposing (FareContract, FareContractState(..), FareTime, TravelRight(..), TravelRightBase, TravelRightCarnet, TravelRightFull, UsedAccess)
import Data.PaymentType as PaymentType
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodeP
import Json.Encode as Encode


port navigateTo : String -> Cmd msg


port reloadPage : () -> Cmd msg


port closeValidityWarning : () -> Cmd msg


port bodyClass : String -> Cmd msg


port receiveTokens : (Encode.Value -> msg) -> Sub msg


port receiveFareContracts : (Encode.Value -> msg) -> Sub msg


port convertTime : ( String, String ) -> Cmd msg


port convertedTime : (( String, Int ) -> msg) -> Sub msg


port onboardingStart : (( String, String, String ) -> msg) -> Sub msg


port onboardingDone : () -> Cmd msg


port onboardingRefreshAuth : () -> Cmd msg


type alias TravelCard =
    { id : Int
    , expires : FareTime
    }


type SignInProvider
    = Password
    | Phone
    | Unknown


type alias SignInMethod =
    { provider : SignInProvider
    , uid : String
    }


type alias Profile =
    { id : String
    , firstName : String
    , lastName : String
    , phone : String
    , email : String
    , travelCard : Maybe TravelCard
    , signInMethods : List SignInMethod
    }


port firestoreReadProfile : (Encode.Value -> msg) -> Sub msg


port firestoreWriteProfile : Encode.Value -> Cmd msg


profileDecoder : Decoder Profile
profileDecoder =
    Decode.succeed Profile
        |> DecodeP.required "id" Decode.string
        |> DecodeP.required "firstName" Decode.string
        |> DecodeP.required "surname" Decode.string
        |> DecodeP.required "phone" Decode.string
        |> DecodeP.required "email" Decode.string
        |> DecodeP.optional "travelcard" (Decode.map Just travelCardDecoder) Nothing
        |> DecodeP.required "signInMethods" (Decode.list signInMethodDecoder)


travelCardDecoder : Decoder TravelCard
travelCardDecoder =
    Decode.succeed TravelCard
        |> DecodeP.required "id" Decode.int
        |> DecodeP.required "expires" timestampDecoder


signInMethodDecoder : Decoder SignInMethod
signInMethodDecoder =
    Decode.succeed SignInMethod
        |> DecodeP.required "providerId" providerDecoder
        |> DecodeP.required "uid" Decode.string


providerFromString : String -> SignInProvider
providerFromString provider =
    case provider of
        "phone" ->
            Phone

        "password" ->
            Password

        _ ->
            Unknown


{-| Decode a provider.
-}
providerDecoder : Decoder SignInProvider
providerDecoder =
    Decode.andThen (providerFromString >> Decode.succeed) Decode.string


encodeProfile : Profile -> Encode.Value
encodeProfile profile =
    Encode.object
        [ ( "id", Encode.string profile.id )
        , ( "firstName", Encode.string profile.firstName )
        , ( "surname", Encode.string profile.lastName )
        , ( "phone", Encode.string profile.phone )
        , ( "email", Encode.string profile.email )
        , ( "travelcard"
          , profile.travelCard
                |> Maybe.map encodeTravelCard
                |> Maybe.withDefault Encode.null
          )
        ]


encodeTravelCard : TravelCard -> Encode.Value
encodeTravelCard travelCard =
    Encode.object
        [ ( "id", Encode.int travelCard.id )
        , ( "expires", Encode.int travelCard.expires.timestamp )
        ]


{-| Whenever the profile has been changed, this event will be fired.
-}
onProfileChange : (Maybe Profile -> msg) -> Sub msg
onProfileChange msg =
    firestoreReadProfile
        (Decode.decodeValue profileDecoder
            >> Result.toMaybe
            >> msg
        )


{-| Save the profile. Note that all fields must be correct.
-}
saveProfile : Profile -> Cmd msg
saveProfile =
    encodeProfile >> firestoreWriteProfile


{-| Decode a fare contract from Firestore.
-}
fareContractDecoder : Decoder FareContract
fareContractDecoder =
    Decode.succeed FareContract
        |> DecodeP.required "created" timestampDecoder
        |> DecodeP.required "version" Decode.string
        |> DecodeP.required "orderId" Decode.string
        |> DecodeP.required "minimumSecurityLevel" Decode.int
        |> DecodeP.required "id" Decode.string
        |> DecodeP.required "travelRights" (Decode.list travelRightDecoder)
        |> DecodeP.required "state" fareContractStateDecoder
        |> DecodeP.optional "qrCode" (Decode.nullable Decode.string) Nothing
        |> DecodeP.required "validFrom" Decode.int
        |> DecodeP.required "validTo" Decode.int
        |> DecodeP.optional "totalAmount" (Decode.map Just Decode.string) Nothing
        |> DecodeP.optional "paymentType"
            (Decode.andThen
                (List.filterMap PaymentType.fromEntur >> Decode.succeed)
                (Decode.list Decode.string)
            )
            []



-- INTERNAL


fareContractStateDecoder : Decoder FareContractState
fareContractStateDecoder =
    Decode.andThen
        (\value ->
            case value of
                0 ->
                    Decode.succeed FareContractStateUnspecified

                1 ->
                    Decode.succeed FareContractStateNotActivated

                2 ->
                    Decode.succeed FareContractStateActivated

                3 ->
                    Decode.succeed FareContractStateCancelled

                4 ->
                    Decode.succeed FareContractStateRefunded

                _ ->
                    Decode.fail "Invalid fare contract state"
        )
        Decode.int


timestampDecoder : Decoder FareTime
timestampDecoder =
    Decode.andThen
        (\timestamp ->
            Decode.andThen
                (\parts ->
                    case parts of
                        [ year, month, day, hour, minute, second ] ->
                            Decode.succeed <| FareTime timestamp year month day hour minute second

                        _ ->
                            Decode.fail "Invalid time parts"
                )
                (Decode.field "parts" (Decode.list Decode.int))
        )
        (Decode.field "timestamp" Decode.int)


travelRightDecoder : Decoder TravelRight
travelRightDecoder =
    Decode.andThen
        (\type_ ->
            case type_ of
                "PreActivatedPeriodTicket" ->
                    Decode.map PeriodTicket fullTravelRightDecoder

                "PreActivatedSingleTicket" ->
                    Decode.map SingleTicket fullTravelRightDecoder

                "CarnetTicket" ->
                    Decode.map CarnetTicket carnetTravelRightDecoder

                _ ->
                    Decode.succeed TravelRightBase
                        |> DecodeP.required "id" Decode.string
                        |> DecodeP.optional "status" (Decode.nullable Decode.int) Nothing
                        |> Decode.map UnknownTicket
        )
        (Decode.field "type" Decode.string)


fullTravelRightDecoder : Decoder TravelRightFull
fullTravelRightDecoder =
    Decode.succeed TravelRightFull
        |> DecodeP.required "id" Decode.string
        |> DecodeP.required "status" Decode.int
        |> DecodeP.required "fareProductRef" Decode.string
        |> DecodeP.required "startDateTime" timestampDecoder
        |> DecodeP.required "endDateTime" timestampDecoder
        |> DecodeP.required "usageValidityPeriodRef" Decode.string
        |> DecodeP.required "userProfileRef" Decode.string
        |> DecodeP.required "authorityRef" Decode.string
        |> DecodeP.required "tariffZoneRefs" (Decode.list Decode.string)


carnetTravelRightDecoder : Decoder TravelRightCarnet
carnetTravelRightDecoder =
    Decode.succeed TravelRightCarnet
        |> DecodeP.required "id" Decode.string
        |> DecodeP.required "status" Decode.int
        |> DecodeP.required "fareProductRef" Decode.string
        |> DecodeP.required "startDateTime" timestampDecoder
        |> DecodeP.required "endDateTime" timestampDecoder
        |> DecodeP.required "usageValidityPeriodRef" Decode.string
        |> DecodeP.required "userProfileRef" Decode.string
        |> DecodeP.required "authorityRef" Decode.string
        |> DecodeP.required "tariffZoneRefs" (Decode.list Decode.string)
        |> DecodeP.required "maximumNumberOfAccesses" Decode.int
        |> DecodeP.required "numberOfUsedAccesses" Decode.int
        |> DecodeP.required "usedAccesses" (Decode.list usedAccessDecoder)


usedAccessDecoder : Decoder UsedAccess
usedAccessDecoder =
    Decode.succeed UsedAccess
        |> DecodeP.required "startDateTime" timestampDecoder
        |> DecodeP.required "endDateTime" timestampDecoder
