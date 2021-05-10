module Page.Overview exposing (Model, Msg(..), init, subscriptions, update, view)

import Base exposing (AppInfo)
import Data.FareContract exposing (FareContract, FareContractState(..), TravelRight(..))
import Data.RefData exposing (LangString(..))
import Data.Ticket exposing (ActiveReservation)
import Data.Webshop exposing (Inspection, Token)
import Environment exposing (Environment)
import Fragment.Icon as Icon
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Extra
import Http
import Json.Decode as Decode exposing (Decoder)
import PageUpdater exposing (PageUpdater)
import Route exposing (Route)
import Service.Misc as MiscService
import Service.Ticket as TicketService
import Shared exposing (Shared)
import Task
import Time
import Ui.Button as B
import Ui.Heading
import Ui.Message as Message
import Ui.Section
import Ui.TicketDetails
import Util.FareContract
import Util.Maybe
import Util.Status exposing (Status(..))


type Msg
    = ReceiveFareContracts (Result Decode.Error (List FareContract))
    | Receipt String
    | ReceiveReceipt (Result Http.Error ())
    | ReceiveTokenPayloads (Result Decode.Error (List ( String, String )))
    | OpenShop
    | OpenHistory
    | OpenSettings
    | OpenEditTravelCard
    | UpdateTime Time.Posix
    | ToggleTicket String
    | AddActiveReservation ActiveReservation
    | Logout


type alias Model =
    { tickets : List FareContract
    , tokens : List Token
    , tokenPayloads : List ( String, String )
    , travelCardId : String
    , inspection : Status Inspection
    , currentTime : Time.Posix
    , expanded : Maybe String
    , reservations : List ActiveReservation
    }


init : ( Model, Cmd Msg )
init =
    ( { tickets = []
      , tokens = []
      , tokenPayloads = []
      , travelCardId = ""
      , inspection = NotLoaded
      , currentTime = Time.millisToPosix 0
      , expanded = Nothing
      , reservations = []
      }
    , Cmd.none
    )


update : Msg -> Environment -> Model -> PageUpdater Model Msg
update msg env model =
    case msg of
        ReceiveFareContracts result ->
            case result of
                Ok fareContracts ->
                    let
                        -- Only store tickets that are valid into the future.
                        tickets =
                            fareContracts
                                |> Util.FareContract.filterValidNow model.currentTime
                                |> List.sortBy (.created >> .timestamp)
                                |> List.reverse

                        orderIds =
                            List.map .orderId tickets

                        activeReservations =
                            model.reservations
                                |> List.filter (\{ reservation } -> not <| List.member reservation.orderId orderIds)
                    in
                        PageUpdater.init { model | tickets = tickets, reservations = activeReservations }

                Err _ ->
                    PageUpdater.init model

        Receipt orderId ->
            PageUpdater.fromPair ( model, sendReceipt env orderId )

        ReceiveReceipt result ->
            case result of
                Ok () ->
                    PageUpdater.init model

                Err _ ->
                    PageUpdater.init model

        ReceiveTokenPayloads result ->
            case result of
                Ok value ->
                    PageUpdater.init { model | tokenPayloads = value }

                Err _ ->
                    PageUpdater.init { model | tokenPayloads = [] }

        OpenShop ->
            PageUpdater.init model
                |> PageUpdater.addGlobalAction GA.OpenShop

        OpenHistory ->
            PageUpdater.init model
                |> PageUpdater.addGlobalAction (GA.RouteTo Route.History)

        OpenSettings ->
            PageUpdater.init model
                |> PageUpdater.addGlobalAction (GA.RouteTo Route.Settings)

        OpenEditTravelCard ->
            PageUpdater.init model
                |> PageUpdater.addGlobalAction GA.OpenEditTravelCard
                |> PageUpdater.addGlobalAction (GA.FocusItem (Just "tkort"))

        UpdateTime posixTime ->
            PageUpdater.init { model | currentTime = posixTime }

        ToggleTicket id ->
            PageUpdater.init
                { model
                    | expanded =
                        if model.expanded == Just id then
                            Nothing

                        else
                            Just id
                }

        AddActiveReservation active ->
            PageUpdater.init { model | reservations = active :: model.reservations }

        Logout ->
            PageUpdater.init model
                |> PageUpdater.addGlobalAction GA.Logout


view : Environment -> AppInfo -> Shared -> Model -> Maybe Route -> Html Msg
view env _ shared model _ =
    case env.customerId of
        Just _ ->
            H.div [ A.class "page page--overview" ]
                [ viewSidebar shared model
                , viewMain shared model
                ]

        Nothing ->
            H.text ""


viewSidebar : Shared -> Model -> Html Msg
viewSidebar shared model =
    H.div [ A.class "sidebar" ]
        [ viewAccountInfo shared model
        , viewActions model
        ]


viewAccountInfo : Shared -> Model -> Html Msg
viewAccountInfo shared _ =
    Ui.Section.init
        |> Ui.Section.setMarginBottom True
        |> Ui.Section.viewWithOptions
            [ Ui.Section.viewPaddedItem
                [ Ui.Heading.component "Min profil"
                , Html.Extra.viewMaybe
                    (\d -> H.p [ A.class "accountInfo__item" ] [ H.text d.phone ])
                    shared.profile
                , shared.profile
                    |> Util.Maybe.flatMap .travelCard
                    |> Maybe.map .id
                    |> Html.Extra.viewMaybe
                        (\id ->
                            H.p [ A.class "accountInfo__item", A.title "t:kort-nummer" ]
                                [ Icon.travelCard
                                , H.text <| String.fromInt id
                                ]
                        )
                ]
            , B.init "Rediger profil"
                |> B.setDisabled False
                |> B.setIcon (Just Icon.edit)
                |> B.setOnClick (Just OpenSettings)
                |> B.tertiary
            , case Util.Maybe.flatMap .travelCard shared.profile of
                Just _ ->
                    Html.Extra.nothing

                _ ->
                    B.init "Legg til t:kort "
                        |> B.setDisabled False
                        |> B.setIcon (Just Icon.travelCard)
                        |> B.setOnClick (Just OpenEditTravelCard)
                        |> B.tertiary
            ]


viewActions : Model -> Html Msg
viewActions _ =
    Ui.Section.view
        [ B.init "Kjøp ny billett"
            |> B.setDisabled False
            |> B.setIcon (Just Icon.ticketAdd)
            |> B.setOnClick (Just OpenShop)
            |> B.tertiary
        , B.init "Kjøpshistorikk"
            |> B.setDisabled False
            |> B.setIcon (Just Icon.tickets)
            |> B.setOnClick (Just OpenHistory)
            |> B.tertiary
        ]


viewMain : Shared -> Model -> Html Msg
viewMain shared model =
    let
        validTickets =
            Util.FareContract.filterValidNow model.currentTime model.tickets

        infoMessage =
            Ui.Section.init
                |> Ui.Section.setMarginBottom True
                |> Ui.Section.viewWithOptions [ Message.info "Billettene som vises her kan ikke brukes i en eventuell kontroll." ]
    in
        H.div [ A.class "main" ]
            [ if List.isEmpty validTickets && List.isEmpty model.reservations then
                H.div [ A.class "pageOverview__empty" ]
                    [ H.img [ A.src "/images/empty-illustration.svg" ] []
                    , H.text "Ingen billetter er tilknyttet din konto."
                    ]

              else
                H.div [] (infoMessage :: viewPending model ++ viewTicketCards shared validTickets model)
            ]


viewPending : Model -> List (Html msg)
viewPending model =
    model.reservations
        |> List.map Ui.TicketDetails.viewActivation


viewTicketCards : Shared -> List FareContract -> Model -> List (Html Msg)
viewTicketCards shared validTickets model =
    validTickets
        |> List.map
            (\f ->
                Ui.TicketDetails.view shared
                    { fareContract = f
                    , open = model.expanded == Just f.orderId
                    , onOpenClick = Just (ToggleTicket f.orderId)
                    , currentTime = model.currentTime
                    }
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ MiscService.receiveTokens (Decode.decodeValue tokenPayloadsDecoder >> ReceiveTokenPayloads)
        , MiscService.receiveFareContracts
            (Decode.decodeValue (Decode.list MiscService.fareContractDecoder)
                >> ReceiveFareContracts
            )
        , Time.every 1000 UpdateTime
        ]



-- INTERNAL


tokenPayloadDecoder : Decoder ( String, String )
tokenPayloadDecoder =
    Decode.map2 Tuple.pair (Decode.index 0 Decode.string) (Decode.index 1 Decode.string)


tokenPayloadsDecoder : Decoder (List ( String, String ))
tokenPayloadsDecoder =
    Decode.list tokenPayloadDecoder


sendReceipt : Environment -> String -> Cmd Msg
sendReceipt env orderId =
    if env.customerEmail == "" then
        Cmd.none

    else
        TicketService.receipt env env.customerEmail orderId
            |> Http.toTask
            |> Task.attempt ReceiveReceipt
