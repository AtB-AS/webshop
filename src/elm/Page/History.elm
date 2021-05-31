module Page.History exposing (Model, Msg, init, subscriptions, update, view)

import Base exposing (AppInfo)
import Data.FareContract exposing (FareContract, FareContractState(..), TravelRight(..))
import Data.RefData exposing (LangString(..))
import Environment exposing (Environment)
import Fragment.Icon as Icon
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Extra
import Http
import Json.Decode as Decode
import List.Extra
import Notification as Notification
import PageUpdater exposing (PageUpdater)
import Route exposing (Route)
import Service.Misc as MiscService exposing (Profile)
import Service.Ticket as TicketService
import Shared exposing (Shared)
import Task
import Ui.Button as B
import Ui.Group
import Ui.Input.Text as T
import Ui.Message as Message
import Ui.Section
import Util.Format as Format


type Msg
    = InputFrom String
    | InputTo String
    | ToggleOrder String
    | ReceiveFareContracts (Result Decode.Error (List FareContract))
    | ProfileChange (Maybe Profile)
    | RequestReceipt String
    | ReceiveReceipt String (Result Http.Error ())


type alias Model =
    { from : Maybe String
    , to : Maybe String
    , orders : List FareContract
    , expanded : Maybe String
    , sendingReceipt : List String
    , profile : Maybe Profile
    }


init : Model
init =
    { from = Nothing
    , to = Nothing
    , orders = []
    , expanded = Nothing
    , sendingReceipt = []
    , profile = Nothing
    }


update : Msg -> Environment -> Model -> PageUpdater Model Msg
update msg env model =
    case msg of
        InputFrom value ->
            PageUpdater.init { model | from = Just value }

        InputTo value ->
            PageUpdater.init { model | to = Just value }

        ToggleOrder id ->
            PageUpdater.init
                { model
                    | expanded =
                        if model.expanded == Just id then
                            Nothing

                        else
                            Just id
                }

        ReceiveFareContracts result ->
            case result of
                Ok fareContracts ->
                    PageUpdater.init
                        { model
                            | orders =
                                fareContracts
                                    |> List.sortBy (.created >> .timestamp)
                                    |> List.reverse
                        }

                Err _ ->
                    PageUpdater.init model

        ProfileChange (Just profile) ->
            PageUpdater.init
                { model | profile = Just profile }

        ProfileChange Nothing ->
            PageUpdater.init { model | profile = Nothing }

        RequestReceipt orderId ->
            case model.profile of
                Just profile ->
                    PageUpdater.fromPair
                        ( { model | sendingReceipt = orderId :: model.sendingReceipt }
                        , TicketService.receipt env profile.email orderId
                            |> Http.toTask
                            |> Task.attempt (ReceiveReceipt orderId)
                        )

                Nothing ->
                    -- TODO: Handle no profile and no email in profile
                    PageUpdater.init model

        ReceiveReceipt orderId _ ->
            -- TODO: Show error
            PageUpdater.init { model | sendingReceipt = List.Extra.remove orderId model.sendingReceipt }
                |> (H.text "Kvitteringen ble sendt til din e-post."
                        |> Message.Valid
                        |> Message.message
                        |> (\s -> Notification.setContent s Notification.init)
                        |> GA.ShowNotification
                        |> PageUpdater.addGlobalAction
                   )


view : Environment -> AppInfo -> Shared -> Model -> Maybe Route -> Html Msg
view _ _ shared model _ =
    H.div [ A.class "page page--history" ]
        [ viewSidebar model
        , viewMain shared model
        ]


viewSidebar : Model -> Html Msg
viewSidebar model =
    Ui.Section.view
        [ Ui.Section.viewHeader "Filtrer på dato"
        , Ui.Section.viewItem
            [ T.init "from"
                |> T.setValue model.from
                |> T.setOnInput (Just InputFrom)
                |> T.setType "date"
                |> T.setPlaceholder "Velg dato"
                |> T.setTitle (Just "Fra")
                |> T.view
            ]
        , Ui.Section.viewItem
            [ T.init "to"
                |> T.setValue model.to
                |> T.setOnInput (Just InputTo)
                |> T.setType "date"
                |> T.setPlaceholder "Velg dato"
                |> T.setTitle (Just "Til")
                |> T.view
            ]
        ]


viewMain : Shared -> Model -> Html Msg
viewMain shared model =
    H.div [ A.class "main" ]
        [ if List.isEmpty model.orders then
            H.div [] [ H.text "Ingen billetter" ]

          else
            H.div [] <| List.map (viewOrder shared model) model.orders
        ]


langString : LangString -> String
langString (LangString _ value) =
    value


fareProductName : Shared -> String -> String
fareProductName shared ref =
    shared.fareProducts
        |> List.filter (.id >> (==) ref)
        |> List.head
        |> Maybe.map (.name >> langString)
        |> Maybe.withDefault "Ukjent product"


userProfileName : Shared -> String -> String
userProfileName shared ref =
    shared.userProfiles
        |> List.filter (.id >> (==) ref)
        |> List.head
        |> Maybe.map (.name >> langString)
        |> Maybe.withDefault "Ukjent brukerprofil"


zoneProfileNames : Shared -> List String -> String
zoneProfileNames shared refs =
    refs
        |> List.map
            (\ref ->
                shared.tariffZones
                    |> List.filter (.id >> (==) ref)
                    |> List.head
                    |> Maybe.map (\zone -> "Sone " ++ langString zone.name)
                    |> Maybe.withDefault "Ukjent sone"
            )
        |> String.join ", "


viewOrder : Shared -> Model -> FareContract -> Html Msg
viewOrder shared model order =
    let
        fareProduct =
            order.travelRights
                |> List.head
                |> Maybe.map
                    (\travelRight ->
                        case travelRight of
                            SingleTicket data ->
                                fareProductName shared data.fareProductRef

                            PeriodTicket data ->
                                fareProductName shared data.fareProductRef

                            UnknownTicket _ ->
                                "Ukjent"
                    )
                |> Maybe.withDefault "Ukjent"

        travellers =
            case List.length order.travelRights of
                0 ->
                    ""

                1 ->
                    ""

                n ->
                    ", " ++ String.fromInt n ++ " reisende"

        expanded =
            model.expanded == Just order.id

        sendingReceipt =
            List.member order.orderId model.sendingReceipt

        missingEmail =
            case model.profile of
                Just profile ->
                    String.isEmpty profile.email

                Nothing ->
                    True
    in
        Ui.Group.view
            { title = Format.date order.created ++ " - " ++ fareProduct ++ travellers
            , id = order.id
            , icon = Nothing
            , value =
                Just
                    (if expanded then
                        "Skjul"

                     else
                        "Vis"
                    )
            , open = expanded
            , disabled = False
            , onOpenClick = Just (ToggleOrder order.id)
            }
            [ Ui.Section.viewPaddedItem
                [ H.label [] [ H.text "Kjøpsinformasjon" ]
                , H.div [ A.class "metadata-list" ]
                    [ H.div [] [ H.text <| "Kjøpt " ++ Format.dateTime order.created ]
                    , H.div [] [ H.text <| "Totalt kr " ++ formatTotal order.totalAmount ]
                    , H.div [] [ H.text <| "Betalt med " ++ formatPaymentType order.paymentType ]
                    , H.div [] [ H.text <| "Ordre-ID: " ++ order.orderId ]
                    ]
                ]
            , Ui.Section.viewPaddedItem
                (List.indexedMap
                    (viewTravelRight shared (List.length order.travelRights))
                    order.travelRights
                )
            , B.init "Be om kvittering på e-post"
                |> B.setDisabled missingEmail
                |> B.setOnClick
                    (if sendingReceipt then
                        Nothing

                     else
                        Just (RequestReceipt order.orderId)
                    )
                |> B.setIcon (Just Icon.rightArrow)
                |> B.tertiary
            , Html.Extra.viewIf missingEmail (Message.warning "Du må legge til epost via profilen din for å kunne sende kvittering.")
            ]


viewTravelRight : Shared -> Int -> Int -> TravelRight -> Html msg
viewTravelRight shared total num travelRight =
    let
        ( fareProduct, userProfile, tariffZone ) =
            case travelRight of
                SingleTicket data ->
                    ( fareProductName shared data.fareProductRef
                    , userProfileName shared data.userProfileRef
                    , zoneProfileNames shared data.tariffZoneRefs
                    )

                PeriodTicket data ->
                    ( fareProductName shared data.fareProductRef
                    , userProfileName shared data.userProfileRef
                    , zoneProfileNames shared data.tariffZoneRefs
                    )

                UnknownTicket _ ->
                    ( "Ukjent", "Ukjent", "Ukjent" )
    in
        H.div []
            [ H.label []
                [ H.text <|
                    if total == 1 then
                        "Billett"

                    else
                        "Billett " ++ String.fromInt (num + 1) ++ " / " ++ String.fromInt total
                ]
            , H.div [ A.class "ticket-list" ]
                [ H.div [] [ H.text fareProduct ]
                , H.div [] [ H.text userProfile ]
                , H.div [] [ H.text tariffZone ]
                ]
            ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ MiscService.receiveFareContracts
            (Decode.decodeValue (Decode.list MiscService.fareContractDecoder)
                >> ReceiveFareContracts
            )
        , MiscService.onProfileChange ProfileChange
        ]



-- INTERNAL


formatTotal : Maybe String -> String
formatTotal value =
    case Maybe.andThen String.toFloat value of
        Just floatValue ->
            Format.float floatValue 2

        Nothing ->
            "??"


formatPaymentType : List String -> String
formatPaymentType types =
    case types of
        [] ->
            "??"

        [ "VIPPS" ] ->
            "Vipps"

        [ "VISA" ] ->
            "bankkort"

        _ ->
            "??"
