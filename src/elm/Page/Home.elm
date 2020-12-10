module Page.Home exposing (Model, Msg(..), init, subscriptions, update, view)

import Base exposing (AppInfo)
import Data.Webshop exposing (FareContract, FareContractState(..), FareProduct, Inspection(..), LangString(..), Profile, Rejection(..), TariffZone, Token, TokenType(..), UserProfile)
import Dict exposing (Dict)
import Environment exposing (Environment)
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Http
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import PageUpdater exposing (PageUpdater)
import Route exposing (Route)
import Service.Misc as MiscService
import Service.Webshop as WebshopService
import Shared exposing (Shared)
import Task
import Time
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
    | DeleteToken String
    | ReceiveDeleteToken (Result Http.Error ())
    | AddQrCode
    | ReceiveAddQrCode (Result Http.Error ())
    | LoadAccount
    | ReceiveTokenPayloads (Result Decode.Error (List ( String, String )))
    | Inspect String
    | ReceiveInspectQrCode (Result Http.Error (List Inspection))
    | OpenShop
    | UpdateTime Time.Posix
    | ToggleTicketView


type TicketView
    = TicketTable
    | TicketCards


type alias Model =
    { tickets : List FareContract
    , tokens : List Token
    , tokenPayloads : List ( String, String )
    , travelCardId : String
    , inspection : Status Inspection
    , currentTime : Time.Posix
    , ticketView : TicketView
    }


init : ( Model, Cmd Msg )
init =
    ( { tickets = []
      , tokens = []
      , tokenPayloads = []
      , travelCardId = ""
      , inspection = NotLoaded
      , currentTime = Time.millisToPosix 0
      , ticketView = TicketCards
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
                    PageUpdater.init { model | tickets = tickets |> List.sortBy .validity |> List.reverse }

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
                    PageUpdater.fromPair ( { model | travelCardId = "" }, fetchTokens env )

                Err _ ->
                    PageUpdater.init model

        DeleteToken tokenId ->
            PageUpdater.fromPair ( model, deleteToken env tokenId )

        ReceiveDeleteToken result ->
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
                    ]
                )

        ReceiveTokenPayloads result ->
            case result of
                Ok value ->
                    PageUpdater.init { model | tokenPayloads = value }

                Err _ ->
                    PageUpdater.init { model | tokenPayloads = [] }

        OpenShop ->
            PageUpdater.init model
                |> PageUpdater.addGlobalAction GA.OpenShop

        UpdateTime posixTime ->
            PageUpdater.init { model | currentTime = posixTime }

        ToggleTicketView ->
            PageUpdater.init
                { model
                    | ticketView =
                        case model.ticketView of
                            TicketCards ->
                                TicketTable

                            TicketTable ->
                                TicketCards
                }


view : Environment -> AppInfo -> Shared -> Model -> Maybe Route -> Html Msg
view env _ shared model _ =
    case env.customerId of
        Just _ ->
            H.div [ A.class "box" ]
                [ H.h2 [] [ H.text "Tickets" ]
                , H.button [ E.onClick FetchTickets ] [ H.text "Refresh" ]
                , H.button [ E.onClick OpenShop ] [ H.text "Buy" ]
                , H.button [ E.onClick ToggleTicketView ] [ H.text "Toggle view" ]
                , if List.isEmpty model.tickets then
                    H.div [] [ H.text "No tickets" ]

                  else if model.ticketView == TicketCards then
                    viewTicketCards shared model

                  else
                    viewTicketTable shared model
                , H.h2 [] [ H.text "Tokens" ]
                , H.button [ E.onClick GetTokens ] [ H.text "Refresh" ]
                , if List.length model.tokens == 0 then
                    H.p [] [ H.text "No tokens." ]

                  else
                    H.div [ A.class "cards" ] <| List.map (viewToken model.tokenPayloads model.currentTime) model.tokens
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


viewTicketCards : Shared -> Model -> Html msg
viewTicketCards shared model =
    H.div [ A.class "cards" ] <| List.map (viewTicketCard shared model) model.tickets


viewTicketTable : Shared -> Model -> Html msg
viewTicketTable shared model =
    H.table []
        [ H.thead []
            [ H.tr []
                [ H.th [] [ H.text "Id" ]
                , H.th [] [ H.text "State" ]
                , H.th [] [ H.text "Validity" ]
                , H.th [] [ H.text "User" ]
                , H.th [] [ H.text "Product" ]
                ]
            ]
        , H.tbody [] <| List.map (viewTicket shared model) model.tickets
        ]


timeLeft : Int -> String
timeLeft time =
    let
        hr =
            time // 3600

        min =
            (time - hr * 3600) // 60

        sec =
            time - hr * 3600 - min * 60

        toStr v suffix =
            if v > 0 then
                String.fromInt v ++ suffix

            else
                ""
    in
        String.join " " [ toStr hr "h", toStr min "m", toStr sec "s" ]


timeAgo : Int -> String
timeAgo time =
    if time >= 86400 then
        String.fromInt (time // 86400) ++ "d"

    else if time >= 3600 then
        String.fromInt (time // 3600) ++ "h"

    else if time >= 60 then
        String.fromInt (time // 60) ++ "m"

    else if time >= 1 then
        String.fromInt time ++ "s"

    else
        "just now"


frequency : List comparable -> Dict comparable Int
frequency =
    List.foldl
        (\item ->
            Dict.update item (Maybe.map ((+) 1) >> Maybe.withDefault 1 >> Just)
        )
        Dict.empty


viewTicket : Shared -> Model -> FareContract -> Html msg
viewTicket shared model fareContract =
    let
        ( _, to ) =
            fareContract.validity

        now =
            Time.posixToMillis model.currentTime // 1000
    in
        H.tr []
            [ H.td [] [ H.text fareContract.id ]
            , H.td [] [ H.text <| fareContractStateToString fareContract.state ]
            , H.td []
                [ if now > to then
                    H.span [ A.style "color" "#f00" ] [ H.text <| "Expired " ++ timeAgo (now - to) ++ " ago" ]

                  else
                    H.span [ A.style "color" "#0f0" ] [ H.text <| "Valid - " ++ timeLeft (to - now) ++ " left" ]
                ]
            , H.td []
                [ fareContract.userProfiles
                    |> frequency
                    |> Dict.map (viewUserProfile shared)
                    |> Dict.values
                    |> String.join ", "
                    |> H.text
                ]
            , H.td []
                [ fareContract.fareProducts
                    |> frequency
                    |> Dict.map (viewFareProduct shared)
                    |> Dict.values
                    |> String.join ", "
                    |> H.text
                ]
            ]


viewValidity : ( Int, Int ) -> Time.Posix -> Html msg
viewValidity ( to, _ ) posixNow =
    let
        now =
            Time.posixToMillis posixNow // 1000
    in
        if now > to then
            H.span [ A.style "color" "#f00" ] [ H.text <| "Expired " ++ timeAgo (now - to) ++ " ago" ]

        else
            H.span [ A.style "color" "#0f0" ] [ H.text <| "Valid - " ++ timeLeft (to - now) ++ " left" ]


viewTicketCard : Shared -> Model -> FareContract -> Html msg
viewTicketCard shared model fareContract =
    let
        userProfiles =
            fareContract.userProfiles
                |> frequency
                |> Dict.map (viewUserProfile shared)
                |> Dict.values

        userInfo =
            case userProfiles of
                [ userProfile ] ->
                    userProfile

                [ _, _ ] ->
                    String.join ", " userProfiles

                _ ->
                    String.fromInt (List.length fareContract.userProfiles) ++ " tickets"

        fareProduct =
            fareContract.fareProducts
                |> frequency
                |> Dict.map (viewFareProduct shared)
                |> Dict.values
                |> String.join ", "
                |> H.text

        fareContractId =
            case String.split ":" fareContract.id of
                [ "ATB", "FareContract", id ] ->
                    id

                _ ->
                    "Unknown"
    in
        H.div [ A.class "card" ]
            [ H.div [ A.class "card-content" ]
                [ H.div [ A.class "card-icon icon-ticket" ] []
                , H.h5 [ A.class "card-name" ] [ fareProduct ]
                , H.h6 [ A.class "card-info" ] [ viewValidity fareContract.validity model.currentTime ]
                ]
            , H.div [ A.class "card-id" ] [ H.text fareContractId ]
            , H.div [ A.class "card-extra" ] [ H.text userInfo ]
            ]


langString : LangString -> String
langString (LangString _ value) =
    value


multiString : Int -> String -> String
multiString count str =
    if String.isEmpty str || count < 1 then
        ""

    else if count == 1 then
        str

    else
        String.fromInt count ++ "× " ++ str


viewUserProfile : Shared -> String -> Int -> String
viewUserProfile shared userProfile count =
    shared.userProfiles
        |> List.filter
            (\entry ->
                entry.id == userProfile
            )
        |> List.map (.name >> langString)
        |> List.head
        |> Maybe.withDefault ""
        |> multiString count


viewFareProduct : Shared -> String -> Int -> String
viewFareProduct shared fareProduct _ =
    shared.fareProducts
        |> List.filter
            (\entry ->
                entry.id == fareProduct
            )
        |> List.map (.name >> langString)
        |> List.head
        |> Maybe.withDefault ""
        |> multiString 1


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


viewInspection : Status Inspection -> Html msg
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


inspectionToString : Inspection -> String
inspectionToString inspection =
    case inspection of
        InspectionGreen ->
            "Green"

        InspectionYellow ->
            "Yellow"

        InspectionRed reason ->
            "Red - " ++ rejectionToString reason


rejectionToString : Rejection -> String
rejectionToString rejection =
    case rejection of
        RejectionNoActiveFareContracts ->
            "No active fare contracts"

        RejectionNoFareContracts ->
            "No fare contracts"

        RejectionFareContractNotActivated ->
            "Fare contract not activated"

        RejectionValidityParametersInvalid ->
            "Validity parameters are invalid"

        RejectionTokenMarkedInactive ->
            "Token marked as inactive"

        RejectionTokenValidityNotStarted ->
            "Token is not valid yet"

        RejectionTokenValidityEnded ->
            "Token is no longer valid"

        RejectionTokenSignatureInvalid ->
            "Token signature is invalid"

        RejectionTokenNotFound ->
            "Token was not found"

        RejectionDifferentTokenType ->
            "Different token type"

        RejectionTokenIdMismatch ->
            "Token id mismatch"

        RejectionTokenActionsMismatch ->
            "Token actions mismatch"


viewToken : List ( String, String ) -> Time.Posix -> Token -> Html Msg
viewToken payloads currentTime token =
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

        ( name, icon, info ) =
            case token.type_ of
                TokenTypeQrPaper _ ->
                    ( "QR token", "icon-token-paper", "" )

                TokenTypeTravelCard cardId ->
                    ( "Travel card"
                    , "icon-token-card"
                    , cardId
                        |> String.toList
                        |> List.map String.fromChar
                        |> List.Extra.groupsOf 4
                        |> List.map (String.join "")
                        |> String.join " "
                    )

                type_ ->
                    ( tokenTypeToString type_, "", "" )
    in
        H.div [ A.class "card" ]
            [ H.div [ A.class "card-content" ]
                [ H.div [ A.class ("card-icon " ++ icon) ] []
                , H.h5 [ A.class "card-name" ] [ H.text name ]
                , H.h6 [ A.class "card-info" ] [ viewValidity token.validity currentTime ]
                ]
            , H.div [ A.class "card-actions" ]
                [ H.div [ A.class "action-content" ]
                    [ H.button [ A.class "btn btn-replace" ] [ H.text "Replace" ]
                    , if payload /= "" then
                        H.button [ A.class "btn btn-inspect", E.onClick (Inspect payload) ] [ H.text "Inspect" ]

                      else
                        H.button [ A.class "btn btn-delete", E.onClick (DeleteToken token.id) ] [ H.text "Delete" ]
                    ]
                ]
            ]


tokenTypeToString : TokenType -> String
tokenTypeToString type_ =
    case type_ of
        TokenTypeUnspecified ->
            "Unspecified"

        TokenTypeQrSmartphone ->
            "QR (smartphone)"

        TokenTypeQrPaper _ ->
            "QR (paper)"

        TokenTypeTravelCard travelCardId ->
            "Travel card - " ++ travelCardId

        TokenTypeReferenceCode ->
            "Reference code"

        TokenTypePlainUnsigned ->
            "Plain unsigned"

        TokenTypeExternal ->
            "External"


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ MiscService.receiveTokens (Decode.decodeValue tokenPayloadsDecoder >> ReceiveTokenPayloads)
        , Time.every 1000 UpdateTime
        ]



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


deleteToken : Environment -> String -> Cmd Msg
deleteToken env tokenId =
    WebshopService.deleteToken env tokenId
        |> Http.toTask
        |> Task.attempt ReceiveDeleteToken


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


checkInspection : List Inspection -> Inspection
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
                (InspectionRed RejectionNoActiveFareContracts)
                (List.head reds)
