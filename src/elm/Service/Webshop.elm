module Service.Webshop exposing
    ( addQrCode
    , addTravelCard
    , getFareContracts
    , getProfile
    , getToken
    , getTokens
    , hello
    , updateProfile
    )

import Data.Webshop exposing (FareContract, FareContractState(..), Profile, Token, TokenAction(..), TokenStatus(..), TokenType(..))
import Environment exposing (Environment)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodeP
import Json.Encode as Encode
import Util.Http as HttpUtil


{-| Say hello. Fails if not logged in.
-}
hello : Environment -> Http.Request ()
hello env =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Atb-Install-Id" env.installId
            , Http.header "Authorization" ("Bearer " ++ env.token)
            ]
        , url = "/api/hellosecure"
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }


getProfile : Environment -> Http.Request Profile
getProfile env =
    HttpUtil.get env "/api/v1/profile" profileDecoder


updateProfile : Environment -> String -> String -> Http.Request ()
updateProfile env firstName lastName =
    let
        payload =
            Encode.object
                [ ( "firstName", Encode.string firstName )
                , ( "surname", Encode.string lastName )
                ]
    in
        HttpUtil.put env "/api/v1/profile" (Http.jsonBody payload) (Decode.succeed ())


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


{-| Get list of tickets.
-}
getFareContracts : Environment -> Http.Request (List FareContract)
getFareContracts env =
    HttpUtil.get env (env.baseUrl ++ "/api/v1/fare-contracts") (Decode.list fareContractDecoder)



-- DECODERS


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
        --|> DecodeP.required "email" Decode.string
        |> DecodeP.hardcoded ""
        |> DecodeP.required "firstName" Decode.string
        |> DecodeP.required "surname" Decode.string
        |> DecodeP.required "enturCustomerNumber" Decode.int


tokenDecoder : Decoder Token
tokenDecoder =
    Decode.succeed Token
        |> DecodeP.required "id" Decode.string
        |> DecodeP.required "allowedActions" (Decode.list tokenActionDecoder)
        |> DecodeP.required "status" tokenStatusDecoder
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
