module Service.Webshop exposing
    ( addQrCode
    , addTravelCard
    , getFareContracts
    , getFareProducts
    , getProfile
    , getTariffZones
    , getToken
    , getTokens
    , getUserProfiles
    , inspectQrCode
    , updateProfile
    )

import Data.Webshop exposing (FareContract, FareContractState(..), FareProduct, Inspection(..), LangString(..), Profile, Rejection(..), TariffZone, Token, TokenAction(..), TokenStatus(..), TokenType(..), UserProfile, UserType(..))
import Environment exposing (Environment)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodeP
import Json.Encode as Encode
import Util.Http as HttpUtil


getProfile : Environment -> Http.Request Profile
getProfile env =
    HttpUtil.get env (env.baseUrl ++ "/api/v1/profile") profileDecoder


updateProfile : Environment -> String -> String -> Http.Request ()
updateProfile env firstName lastName =
    let
        payload =
            Encode.object
                [ ( "firstName", Encode.string firstName )
                , ( "surname", Encode.string lastName )
                ]
    in
        HttpUtil.put env (env.baseUrl ++ "/api/v1/profile") (Http.jsonBody payload) (Decode.succeed ())


getTokens : Environment -> Http.Request (List Token)
getTokens env =
    HttpUtil.get env "/api/v1/tokens" (Decode.list tokenDecoder)


getToken : Environment -> String -> Http.Request Token
getToken env id =
    HttpUtil.get env ("/api/v1/tokens/" ++ id) tokenDecoder


addTravelCard : Environment -> String -> Http.Request ()
addTravelCard env id =
    let
        payload =
            Encode.object [ ( "travelCardId", Encode.string id ) ]
    in
        HttpUtil.post env "/api/v1/tokens/travelcard" (Http.jsonBody payload) (Decode.succeed ())


addQrCode : Environment -> Http.Request ()
addQrCode env =
    HttpUtil.post env "/api/v1/tokens/qrcode" Http.emptyBody (Decode.succeed ())


inspectQrCode : Environment -> String -> Http.Request (List Inspection)
inspectQrCode env tokenPayload =
    let
        payload =
            Encode.object [ ( "token", Encode.string tokenPayload ) ]
    in
        HttpUtil.post env "/api/v1/inspection/qrcode" (Http.jsonBody payload) (Decode.list inspectionDecoder)


{-| Get list of tickets.
-}
getFareContracts : Environment -> Http.Request (List FareContract)
getFareContracts env =
    HttpUtil.get env (env.baseUrl ++ "/api/v1/fare-contracts") (Decode.list fareContractDecoder)


{-| Get list of tariff zones.
-}
getTariffZones : Environment -> Http.Request (List TariffZone)
getTariffZones env =
    HttpUtil.get env (env.baseUrl ++ "/api/v1/reference-data/ATB/tariff-zones") (Decode.list tariffZoneDecoder)


{-| Get list of fare products.
-}
getFareProducts : Environment -> Http.Request (List FareProduct)
getFareProducts env =
    HttpUtil.get env (env.baseUrl ++ "/api/v1/reference-data/ATB/preassigned-fare-products") (Decode.list fareProductDecoder)


{-| Get list of user profiles.
-}
getUserProfiles : Environment -> Http.Request (List UserProfile)
getUserProfiles env =
    HttpUtil.get env (env.baseUrl ++ "/api/v1/reference-data/ATB/user-profiles") (Decode.list userProfileDecoder)



-- DECODERS


langStringDecoder : Decoder LangString
langStringDecoder =
    Decode.succeed LangString
        |> DecodeP.required "lang" Decode.string
        |> DecodeP.required "value" Decode.string


tariffZoneDecoder : Decoder TariffZone
tariffZoneDecoder =
    Decode.succeed TariffZone
        |> DecodeP.required "id" Decode.string
        |> DecodeP.required "name" langStringDecoder


fareProductDecoder : Decoder FareProduct
fareProductDecoder =
    Decode.succeed FareProduct
        |> DecodeP.required "id" Decode.string
        |> DecodeP.required "name" langStringDecoder
        |> DecodeP.required "description" langStringDecoder
        |> DecodeP.required "alternativeNames" (Decode.list langStringDecoder)


userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    Decode.succeed UserProfile
        |> DecodeP.required "id" Decode.string
        |> DecodeP.required "name" langStringDecoder
        |> DecodeP.required "description" langStringDecoder
        |> DecodeP.required "alternativeNames" (Decode.list langStringDecoder)
        |> DecodeP.custom
            (Decode.succeed Tuple.pair
                |> DecodeP.optional "minAge" Decode.int 0
                |> DecodeP.required "maxAge" Decode.int
            )
        |> DecodeP.required "userType" userTypeDecoder


userTypeDecoder : Decoder UserType
userTypeDecoder =
    Decode.andThen
        (\userType ->
            case userType of
                1 ->
                    Decode.succeed UserTypeAdult

                2 ->
                    Decode.succeed UserTypeChild

                3 ->
                    Decode.succeed UserTypeInfant

                4 ->
                    Decode.succeed UserTypeSenior

                5 ->
                    Decode.succeed UserTypeStudent

                6 ->
                    Decode.succeed UserTypeYoungPerson

                7 ->
                    Decode.succeed UserTypeSchoolPupil

                8 ->
                    Decode.succeed UserTypeMilitary

                9 ->
                    Decode.succeed UserTypeDisabled

                10 ->
                    Decode.succeed UserTypeDisabledCompanion

                11 ->
                    Decode.succeed UserTypeJobSeeker

                12 ->
                    Decode.succeed UserTypeEmployee

                13 ->
                    Decode.succeed UserTypeAnimal

                14 ->
                    Decode.succeed UserTypeAnyone

                _ ->
                    Decode.fail "Invalid user type"
        )
        Decode.int


inspectionDecoder : Decoder Inspection
inspectionDecoder =
    Decode.andThen
        (\result ->
            case result of
                1 ->
                    Decode.succeed InspectionGreen

                2 ->
                    Decode.succeed InspectionYellow

                3 ->
                    Decode.field "reason" (Decode.map InspectionRed rejectionReasonDecoder)

                _ ->
                    Decode.fail "Invalid inspection result"
        )
        (Decode.field "result" Decode.int)


rejectionReasonDecoder : Decoder Rejection
rejectionReasonDecoder =
    Decode.andThen
        (\reason ->
            case reason of
                1 ->
                    Decode.succeed RejectionNoActiveFareContracts

                2 ->
                    Decode.succeed RejectionNoFareContracts

                3 ->
                    Decode.succeed RejectionFareContractNotActivated

                4 ->
                    Decode.succeed RejectionValidityParametersInvalid

                100 ->
                    Decode.succeed RejectionTokenMarkedInactive

                101 ->
                    Decode.succeed RejectionTokenValidityNotStarted

                102 ->
                    Decode.succeed RejectionTokenValidityEnded

                103 ->
                    Decode.succeed RejectionTokenSignatureInvalid

                104 ->
                    Decode.succeed RejectionTokenNotFound

                105 ->
                    Decode.succeed RejectionDifferentTokenType

                106 ->
                    Decode.succeed RejectionTokenIdMismatch

                107 ->
                    Decode.succeed RejectionTokenActionsMismatch

                _ ->
                    Decode.fail "Invalid rejection reason"
        )
        Decode.int


fareContractDecoder : Decoder FareContract
fareContractDecoder =
    Decode.succeed FareContract
        |> DecodeP.required "id" Decode.string
        |> DecodeP.required "state"
            (Decode.andThen
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
            )


profileDecoder : Decoder Profile
profileDecoder =
    Decode.succeed Profile
        |> DecodeP.optional "email" Decode.string ""
        |> DecodeP.required "firstName" Decode.string
        |> DecodeP.required "surname" Decode.string
        |> DecodeP.required "enturCustomerNumber" Decode.int


tokenDecoder : Decoder Token
tokenDecoder =
    Decode.succeed Token
        |> DecodeP.required "id" Decode.string
        |> DecodeP.required "allowedActions" (Decode.list tokenActionDecoder)
        |> DecodeP.optional "status" tokenStatusDecoder TokenStatusUnspecified
        |> DecodeP.required "type" tokenTypeDecoder


tokenActionDecoder : Decoder TokenAction
tokenActionDecoder =
    Decode.andThen
        (\value ->
            case value of
                0 ->
                    Decode.succeed TokenActionUnspecified

                1 ->
                    Decode.succeed TokenActionTicketTransfer

                2 ->
                    Decode.succeed TokenActionAddRemoveToken

                3 ->
                    Decode.succeed TokenActionIdentification

                4 ->
                    Decode.succeed TokenActionTicketInspection

                5 ->
                    Decode.succeed TokenActionGetFareContracts

                6 ->
                    Decode.succeed TokenActionTravelCard

                _ ->
                    Decode.fail "Invalid token action"
        )
        Decode.int


tokenStatusDecoder : Decoder TokenStatus
tokenStatusDecoder =
    Decode.andThen
        (\value ->
            case value of
                0 ->
                    Decode.succeed TokenStatusUnspecified

                1 ->
                    Decode.succeed TokenStatusActive

                2 ->
                    Decode.succeed TokenStatusInactive

                3 ->
                    Decode.succeed TokenStatusOther

                _ ->
                    Decode.fail "Invalid token status"
        )
        Decode.int


tokenTypeDecoder : Decoder TokenType
tokenTypeDecoder =
    Decode.andThen
        (\value ->
            case value of
                0 ->
                    Decode.succeed TokenTypeUnspecified

                1 ->
                    Decode.succeed TokenTypeQrSmartphone

                2 ->
                    Decode.succeed TokenTypeQrPaper

                3 ->
                    Decode.succeed TokenTypeTravelCard

                4 ->
                    Decode.succeed TokenTypeReferenceCode

                5 ->
                    Decode.succeed TokenTypePlainUnsigned

                6 ->
                    Decode.succeed TokenTypeExternal

                _ ->
                    Decode.fail "Invalid token type"
        )
        Decode.int
