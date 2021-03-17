module Page.History exposing (Model, Msg, init, subscriptions, update, view)

import Base exposing (AppInfo)
import Data.FareContract exposing (FareContract, FareContractState(..), FareTime, TravelRight(..))
import Data.RefData exposing (LangString(..))
import Environment exposing (Environment)
import Fragment.Icon as Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Decode
import PageUpdater exposing (PageUpdater)
import Route exposing (Route)
import Service.Misc as MiscService
import Shared exposing (Shared)
import Util.Format as Format exposing (date)


type Msg
    = InputFrom String
    | InputTo String
    | ToggleOrder String
    | ReceiveFareContracts (Result Decode.Error (List FareContract))


type alias Model =
    { from : Maybe String
    , to : Maybe String
    , orders : List FareContract
    , expanded : Maybe String
    }


init : Model
init =
    { from = Nothing
    , to = Nothing
    , orders = []
    , expanded = Nothing
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


view : Environment -> AppInfo -> Shared -> Model -> Maybe Route -> Html Msg
view env _ shared model _ =
    H.div [ A.class "page-history" ]
        [ H.div [ A.class "sidebar" ] [ viewSidebar model ]
        , H.div [ A.class "main" ] [ viewMain shared model ]
        ]


viewSidebar : Model -> Html Msg
viewSidebar model =
    H.div [ A.class "section-box" ]
        [ H.div [ A.class "section-header" ] [ H.text "Filtrer på dato" ]
        , textInput (Maybe.withDefault "" model.from)
            InputFrom
            "Fra"
            "Velg dato"
        , textInput (Maybe.withDefault "" model.to)
            InputTo
            "Til"
            "Velg dato"
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
    in
        H.div [ A.class "section-box" ]
            (H.div [ A.class "order-header", E.onClick (ToggleOrder order.id) ]
                [ H.div [] [ H.text <| Format.date order.created ++ " - " ++ fareProduct ++ travellers ]
                , H.div [ A.style "display" "flex" ] <|
                    if expanded then
                        [ H.span [ A.style "margin-right" "12px" ] [ H.text "Skjul" ]
                        , Icon.wrapper 20 Icon.upArrow
                        ]

                    else
                        [ H.span [ A.style "margin-right" "12px" ] [ H.text "Vis" ]
                        , Icon.wrapper 20 Icon.downArrow
                        ]
                ]
                :: (if expanded then
                        List.concat
                            [ [ H.div []
                                    [ H.label [] [ H.text "Kjøpsinformasjon" ]
                                    , H.div [ A.class "metadata-list" ]
                                        [ H.div [] [ H.text <| "Kjøpt " ++ Format.dateTime order.created ]
                                        , H.div [] [ H.text "Totalt kr ??,00" ]
                                        , H.div [] [ H.text "Betalt med ???" ]
                                        , H.div [] [ H.text <| "Ordre-ID: " ++ order.orderId ]
                                        ]
                                    ]
                              ]
                            , List.indexedMap
                                (viewTravelRight shared (List.length order.travelRights))
                                order.travelRights
                            , [ H.div [ A.style "display" "flex" ]
                                    [ H.div
                                        [ A.style "flex-grow" "1"
                                        , A.style "font-weight" "500"
                                        ]
                                        [ H.text "Be om kvittering på e-post" ]
                                    , Icon.rightArrow
                                    ]
                              ]
                            ]

                    else
                        []
                   )
            )


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
    MiscService.receiveFareContracts
        (Decode.decodeValue (Decode.list MiscService.fareContractDecoder)
            >> ReceiveFareContracts
        )



-- INTERNAL


textInput : String -> (String -> msg) -> String -> String -> Html msg
textInput value action title placeholder =
    H.div []
        [ H.label [] [ H.text title ]
        , H.div []
            [ H.input
                [ A.type_ "text"
                , A.placeholder placeholder
                , E.onInput action
                , A.value value
                ]
                []
            ]
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
