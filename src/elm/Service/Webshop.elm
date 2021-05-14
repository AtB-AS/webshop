module Service.Webshop exposing
    ( addQrCode
    , addTravelCard
    , deleteToken
    , deleteTravelCard
    , getFareContracts
    , getProfile
    , getToken
    , getTokens
    , inspectQrCode
    , save
    , travelCardErrorDecoder
    , updateEmail
    , updateProfile
    )

import Data.Webshop exposing (FareContract, FareContractState(..), Inspection(..), Profile, Rejection(..), Token, TokenAction(..), TokenStatus(..), TokenType(..))
import Environment exposing (Environment)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodeP
import Json.Encode as Encode
import Util.Encode as EncodeUtil
import Util.Http as HttpUtil


getProfile : Environment -> Http.Request Profile
getProfile env =
    HttpUtil.get env (env.baseUrl ++ "/api/v1/profile") (Http.expectJson profileDecoder)


updateProfile : Environment -> String -> String -> Http.Request ()
updateProfile env firstName lastName =
    let
        payload =
            Encode.object
                [ ( "firstName", Encode.string firstName )
                , ( "surname", Encode.string lastName )
                ]
    in
        HttpUtil.patch env (env.baseUrl ++ "/webshop/v1/profile") (Http.jsonBody payload) (Http.expectStringResponse (\_ -> Ok ()))


updateEmail : Environment -> String -> Http.Request ()
updateEmail env email =
    let
        payload =
            Encode.object
                [ ( "email", Encode.string email )
                ]
    in
        HttpUtil.patch env (env.baseUrl ++ "/webshop/v1/profile") (Http.jsonBody payload) (Http.expectStringResponse (\_ -> Ok ()))


getTokens : Environment -> Http.Request (List Token)
getTokens env =
    HttpUtil.get env (env.baseUrl ++ "/api/v1/tokens") (Http.expectJson (Decode.list tokenDecoder))


getToken : Environment -> String -> Http.Request Token
getToken env id =
    HttpUtil.get env (env.baseUrl ++ "/api/v1/tokens/" ++ id) (Http.expectJson tokenDecoder)


addTravelCard : Environment -> String -> Http.Request ()
addTravelCard env id =
    let
        payload =
            Encode.object [ ( "id", Encode.string id ) ]
    in
        HttpUtil.post env (env.baseUrl ++ "/webshop/v1/travelcard") (Http.jsonBody payload) (Http.expectStringResponse (\_ -> Ok ()))


deleteTravelCard : Environment -> String -> Http.Request ()
deleteTravelCard env id =
    let
        payload =
            Encode.object [ ( "id", Encode.string id ) ]
    in
        HttpUtil.delete env (env.baseUrl ++ "/webshop/v1/travelcard") (Http.jsonBody payload) (Http.expectStringResponse (\_ -> Ok ()))


deleteToken : Environment -> String -> Http.Request ()
deleteToken env tokenId =
    HttpUtil.delete env (env.baseUrl ++ "/api/v1/tokens/" ++ tokenId) Http.emptyBody (Http.expectStringResponse (\_ -> Ok ()))


addQrCode : Environment -> Http.Request ()
addQrCode env =
    HttpUtil.post env (env.baseUrl ++ "/api/v1/tokens/qrcode") Http.emptyBody (Http.expectJson (Decode.succeed ()))


inspectQrCode : Environment -> String -> Http.Request (List Inspection)
inspectQrCode env tokenPayload =
    let
        payload =
            Encode.object [ ( "token", Encode.string tokenPayload ) ]
    in
        HttpUtil.post env (env.baseUrl ++ "/api/v1/inspection/qrcode") (Http.jsonBody payload) (Http.expectJson (Decode.list inspectionDecoder))


{-| Get list of tickets.
-}
getFareContracts : Environment -> Http.Request (List FareContract)
getFareContracts env =
    HttpUtil.get env (env.baseUrl ++ "/api/v1/fare-contracts") (Http.expectJson (Decode.list fareContractDecoder))


save : Environment -> String -> String -> Maybe String -> Maybe String -> Http.Request ()
save env firstName lastName phone email =
    let
        payload =
            Encode.object
                [ ( "firstName", Encode.string firstName )
                , ( "surname", Encode.string lastName )
                , ( "phone", EncodeUtil.maybe phone Encode.string )
                , ( "email", EncodeUtil.maybe email Encode.string )
                ]
    in
        HttpUtil.post env
            (env.baseUrl ++ "/webshop/v1/register")
            (Http.jsonBody payload)
            (Http.expectJson (Decode.succeed ()))



-- DECODERS


travelCardErrorDecoder : String -> Result Decode.Error String
travelCardErrorDecoder =
    Decode.decodeString (Decode.field "error" Decode.string)


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


fareContractDecoder : Decoder FareContract
fareContractDecoder =
    Decode.succeed FareContract
        |> DecodeP.required "id" Decode.string
        |> DecodeP.custom
            (Decode.succeed Tuple.pair
                |> DecodeP.required "validFrom" Decode.int
                |> DecodeP.required "validTo" Decode.int
            )
        |> DecodeP.required "state" fareContractStateDecoder
        |> DecodeP.optional "userProfileRefs" (Decode.list Decode.string) []
        |> DecodeP.optional "fareProductRefs" (Decode.list Decode.string) []


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
        |> DecodeP.custom tokenTypeDecoder
        |> DecodeP.custom
            (Decode.succeed Tuple.pair
                |> DecodeP.required "validityStart" Decode.int
                |> DecodeP.optional "validityEnd" Decode.int 0
            )


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
                        |> DecodeP.required "content" Decode.string

                3 ->
                    Decode.succeed TokenTypeTravelCard
                        |> DecodeP.required "content" Decode.string

                4 ->
                    Decode.succeed TokenTypeReferenceCode

                5 ->
                    Decode.succeed TokenTypePlainUnsigned

                6 ->
                    Decode.succeed TokenTypeExternal

                _ ->
                    Decode.fail "Invalid token type"
        )
        (Decode.field "type" Decode.int)
