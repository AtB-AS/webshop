module Page.Home exposing (Model, Msg(..), init, subscriptions, update, view)

import Base exposing (AppInfo)
import Data.Webshop exposing (FareContract, FareContractState(..), FareProduct, InspectionResult(..), Profile, RejectionReason(..), TariffZone, Token, TokenType(..), UserProfile)
import Environment exposing (Environment)
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Http
import Json.Decode as Decode exposing (Decoder)
import PageUpdater exposing (PageUpdater)
import Route exposing (Route)
import Service.Misc as MiscService
import Service.Webshop as WebshopService
import Task
import Util.Status exposing (Status(..))
import Util.Task as TaskUtil


type Msg
    = FetchTickets
    | ReceiveTickets (Result Http.Error (List FareContract))
    | GetTokens
    | ReceiveTokens (Result Http.Error (List Token))
    | UpdateTravelCardId String
    | AddTravelCard
    | ReceiveAddTravelCard (Result Http.Error ())
    | AddQrCode
    | ReceiveAddQrCode (Result Http.Error ())
    | LoadAccount
    | ReceiveTokenPayloads (Result Decode.Error (List ( String, String )))
    | Inspect String
    | ReceiveInspectQrCode (Result Http.Error (List InspectionResult))
    | ReceiveTariffZones (Result Http.Error (List TariffZone))
    | ReceiveFareProducts (Result Http.Error (List FareProduct))
    | ReceiveUserProfiles (Result Http.Error (List UserProfile))
    | OpenShop


type alias Model =
    { tickets : List FareContract
    , tokens : List Token
    , tokenPayloads : List ( String, String )
    , travelCardId : String
    , inspection : Status InspectionResult
    , tariffZones : List TariffZone
    , fareProducts : List FareProduct
    , userProfiles : List UserProfile
    }


init : ( Model, Cmd Msg )
init =
    ( { tickets = []
      , tokens = []
      , tokenPayloads = []
      , travelCardId = ""
      , inspection = NotLoaded
      , tariffZones = []
      , fareProducts = []
      , userProfiles = []
      }
    , Cmd.none
    )


update : Msg -> Environment -> Model -> PageUpdater Model Msg
update msg env model =
    case msg of
        FetchTickets ->
            PageUpdater.fromPair
                ( model
                , env.customerId
                    |> Maybe.map (fetchTickets env)
                    |> Maybe.withDefault Cmd.none
                )

        ReceiveTickets result ->
            case result of
                Ok tickets ->
                    PageUpdater.init { model | tickets = tickets }

                Err err ->
                    PageUpdater.init model

        Inspect tokenPayload ->
            PageUpdater.fromPair ( { model | inspection = Loading Nothing }, inspect env tokenPayload )

        ReceiveInspectQrCode result ->
            case result of
                Ok [] ->
                    PageUpdater.init { model | inspection = NotLoaded }

                Ok inspection ->
                    PageUpdater.init { model | inspection = Loaded <| checkInspection inspection }

                Err err ->
                    PageUpdater.init { model | inspection = Failed "Failed to perform inspection" }

        GetTokens ->
            PageUpdater.fromPair ( model, fetchTokens env )

        ReceiveTokens result ->
            case result of
                Ok tokens ->
                    PageUpdater.init { model | tokens = tokens }

                Err _ ->
                    PageUpdater.init model

        UpdateTravelCardId value ->
            PageUpdater.init { model | travelCardId = value }

        AddTravelCard ->
            PageUpdater.fromPair ( model, addTravelCard env model.travelCardId )

        ReceiveAddTravelCard result ->
            case result of
                Ok () ->
                    PageUpdater.fromPair ( model, fetchTokens env )

                Err _ ->
                    PageUpdater.init model

        AddQrCode ->
            PageUpdater.fromPair ( model, addQrCode env )

        ReceiveAddQrCode result ->
            case result of
                Ok () ->
                    PageUpdater.fromPair ( model, fetchTokens env )

                Err _ ->
                    PageUpdater.init model

        LoadAccount ->
            PageUpdater.fromPair
                ( model
                , Cmd.batch
                    [ TaskUtil.doTask GetTokens
                    , TaskUtil.doTask FetchTickets
                    , fetchTariffZones env
                    , fetchFareProducts env
                    , fetchUserProfiles env
                    ]
                )

        ReceiveTokenPayloads result ->
            case result of
                Ok value ->
                    PageUpdater.init { model | tokenPayloads = value }

                Err _ ->
                    PageUpdater.init { model | tokenPayloads = [] }

        ReceiveTariffZones result ->
            case result of
                Ok value ->
                    PageUpdater.init { model | tariffZones = value }

                Err _ ->
                    PageUpdater.init model

        ReceiveFareProducts result ->
            case result of
                Ok value ->
                    PageUpdater.init { model | fareProducts = value }

                Err _ ->
                    PageUpdater.init model

        ReceiveUserProfiles result ->
            case result of
                Ok value ->
                    PageUpdater.init { model | userProfiles = value }

                Err _ ->
                    PageUpdater.init model

        OpenShop ->
            PageUpdater.init model
                |> PageUpdater.addGlobalAction GA.OpenShop


view : Environment -> AppInfo -> Model -> Maybe Route -> Html Msg
view env _ model _ =
    case env.customerId of
        Just _ ->
            H.div [ A.class "box" ]
                [ H.h2 [] [ H.text "Tickets" ]
                , H.button [ E.onClick FetchTickets ] [ H.text "Refresh" ]
                , H.button [ E.onClick OpenShop ] [ H.text "Buy" ]
                , H.ol [] <| List.map viewTicket model.tickets
                , H.h2 [] [ H.text "Tokens" ]
                , H.button [ E.onClick GetTokens ] [ H.text "Refresh" ]
                , if List.length model.tokens == 0 then
                    H.p [] [ H.text "No tokens." ]

                  else
                    H.ol [] <| List.map (viewToken model.tokenPayloads) model.tokens
                , viewInspection model.inspection
                , if List.isEmpty model.tokens then
                    H.div []
                        [ H.h3 [] [ H.text "Add QR token" ]
                        , H.button
                            [ E.onClick AddQrCode ]
                            [ H.text "Add" ]
                        ]

                  else
                    H.text ""
                , H.h3 [] [ H.text "Add travel card" ]
                , H.input
                    [ A.value model.travelCardId
                    , E.onInput UpdateTravelCardId
                    , A.placeholder "Travel card id"
                    ]
                    []
                , H.button
                    [ A.disabled (String.trim model.travelCardId == "")
                    , E.onClick AddTravelCard
                    ]
                    [ H.text "Add" ]
                ]

        Nothing ->
            H.div [ A.class "box" ]
                [ H.h2 [] [ H.text "Not logged in" ]
                , H.p [] [ H.text "You need to log in." ]
                ]


viewTicket : FareContract -> Html msg
viewTicket fareContract =
    H.li []
        [ H.div [] [ H.text <| "Id: " ++ fareContract.id ]
        , H.div [] [ H.text <| "State: " ++ fareContractStateToString fareContract.state ]
        ]


fareContractStateToString : FareContractState -> String
fareContractStateToString state =
    case state of
        FareContractStateUnspecified ->
            "Unspecified"

        FareContractStateNotActivated ->
            "Not activated"

        FareContractStateActivated ->
            "Activated"

        FareContractStateCancelled ->
            "Cancelled"

        FareContractStateRefunded ->
            "Refunded"


viewInspection : Status InspectionResult -> Html msg
viewInspection inspection =
    case inspection of
        NotLoaded ->
            H.text ""

        Loading _ ->
            H.div [] [ H.text "Inspection: Starting inspection..." ]

        Loaded result ->
            H.div [] [ H.text ("Inspection: " ++ inspectionToString result) ]

        Failed error ->
            H.div [] [ H.text ("Inspection: Error - " ++ error) ]


inspectionToString : InspectionResult -> String
inspectionToString inspection =
    case inspection of
        InspectionGreen ->
            "Green"

        InspectionYellow ->
            "Yellow"

        InspectionRed reason ->
            "Red - " ++ rejectionToString reason


rejectionToString : RejectionReason -> String
rejectionToString rejection =
    case rejection of
        RejectionReasonNoActiveFareContracts ->
            "No active fare contracts"

        RejectionReasonNoFareContracts ->
            "No fare contracts"

        RejectionReasonFareContractNotActivated ->
            "Fare contract not activated"

        RejectionReasonValidityParametersInvalid ->
            "Validity parameters are invalid"

        RejectionReasonTokenMarkedInactive ->
            "Token marked as inactive"

        RejectionReasonTokenValidityNotStarted ->
            "Token is not valid yet"

        RejectionReasonTokenValidityEnded ->
            "Token is no longer valid"

        RejectionReasonTokenSignatureInvalid ->
            "Token signature is invalid"

        RejectionReasonTokenNotFound ->
            "Token was not found"

        RejectionReasonDifferentTokenType ->
            "Different token type"

        RejectionReasonTokenIdMismatch ->
            "Token id mismatch"

        RejectionReasonTokenActionsMismatch ->
            "Token actions mismatch"


viewToken : List ( String, String ) -> Token -> Html Msg
viewToken payloads token =
    let
        payload =
            payloads
                |> List.filterMap
                    (\( id, value ) ->
                        if id == token.id then
                            Just value

                        else
                            Nothing
                    )
                |> List.head
                |> Maybe.withDefault ""
    in
        H.li []
            [ H.div [] [ H.text <| "Id: " ++ token.id ]
            , H.div [] [ H.text <| "Type: " ++ tokenTypeToString token.type_ ]
            , if payload /= "" then
                H.div [] [ H.button [ E.onClick (Inspect payload) ] [ H.text "Inspect" ] ]

              else
                H.text ""
            ]


tokenTypeToString : TokenType -> String
tokenTypeToString type_ =
    case type_ of
        TokenTypeUnspecified ->
            "Unspecified"

        TokenTypeQrSmartphone ->
            "QR (smartphone)"

        TokenTypeQrPaper ->
            "QR (paper)"

        TokenTypeTravelCard ->
            "Travel card"

        TokenTypeReferenceCode ->
            "Reference code"

        TokenTypePlainUnsigned ->
            "Plain unsigned"

        TokenTypeExternal ->
            "External"


subscriptions : Model -> Sub Msg
subscriptions _ =
    MiscService.receiveTokens (Decode.decodeValue tokenPayloadsDecoder >> ReceiveTokenPayloads)



-- INTERNAL


tokenPayloadDecoder : Decoder ( String, String )
tokenPayloadDecoder =
    Decode.map2 Tuple.pair (Decode.index 0 Decode.string) (Decode.index 1 Decode.string)


tokenPayloadsDecoder : Decoder (List ( String, String ))
tokenPayloadsDecoder =
    Decode.list tokenPayloadDecoder


fetchTickets : Environment -> String -> Cmd Msg
fetchTickets env customerId =
    WebshopService.getFareContracts env
        |> Http.toTask
        |> Task.attempt ReceiveTickets


fetchTokens : Environment -> Cmd Msg
fetchTokens env =
    WebshopService.getTokens env
        |> Http.toTask
        |> Task.attempt ReceiveTokens


addTravelCard : Environment -> String -> Cmd Msg
addTravelCard env id =
    WebshopService.addTravelCard env id
        |> Http.toTask
        |> Task.attempt ReceiveAddTravelCard


addQrCode : Environment -> Cmd Msg
addQrCode env =
    WebshopService.addQrCode env
        |> Http.toTask
        |> Task.attempt ReceiveAddQrCode


inspect : Environment -> String -> Cmd Msg
inspect env tokenPayload =
    WebshopService.inspectQrCode env tokenPayload
        |> Http.toTask
        |> Task.attempt ReceiveInspectQrCode


fetchTariffZones : Environment -> Cmd Msg
fetchTariffZones env =
    WebshopService.getTariffZones env
        |> Http.toTask
        |> Task.attempt ReceiveTariffZones


fetchFareProducts : Environment -> Cmd Msg
fetchFareProducts env =
    WebshopService.getFareProducts env
        |> Http.toTask
        |> Task.attempt ReceiveFareProducts


fetchUserProfiles : Environment -> Cmd Msg
fetchUserProfiles env =
    WebshopService.getUserProfiles env
        |> Http.toTask
        |> Task.attempt ReceiveUserProfiles


checkInspection : List InspectionResult -> InspectionResult
checkInspection results =
    let
        greens =
            List.filter ((==) InspectionGreen) results

        yellows =
            List.filter ((==) InspectionYellow) results

        reds =
            List.filter
                (\result ->
                    case result of
                        InspectionRed _ ->
                            True

                        _ ->
                            False
                )
                results
    in
        if List.length greens > 0 then
            InspectionGreen

        else if List.length yellows > 0 then
            InspectionYellow

        else
            Maybe.withDefault
                (InspectionRed RejectionReasonNoActiveFareContracts)
                (List.head reds)
