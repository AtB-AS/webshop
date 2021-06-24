module Page.Overview exposing (Model, Msg(..), init, subscriptions, update, view)

import Base exposing (AppInfo)
import Data.FareContract exposing (FareContract, FareContractState(..), TravelRight(..))
import Data.RefData exposing (DistributionChannel(..), LangString(..), ProductType(..))
import Data.Ticket exposing (PaymentStatus, Reservation, ReservationStatus(..))
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
import Process
import Route exposing (Route)
import Service.Misc as MiscService
import Service.Ticket as TicketService
import Shared exposing (Shared)
import Task
import Time
import Ui.Button as B
import Ui.Heading
import Ui.Message as Message
import Ui.PageHeader as PH
import Ui.Section
import Ui.TicketDetails
import Ui.TravelCardText
import Util.FareContract
import Util.Maybe
import Util.PhoneNumber
import Util.Status exposing (Status(..))


type Msg
    = OnEnterPage
    | ReceiveFareContracts (Result Decode.Error (List FareContract))
    | Receipt String
    | ReceiveReceipt (Result Http.Error ())
    | ReceiveTokenPayloads (Result Decode.Error (List ( String, String )))
    | OpenHistory
    | OpenSettings
    | OpenEditTravelCard
    | UpdateTime Time.Posix
    | AdjustTimeZone Time.Zone
    | ToggleTicket String
    | AddActiveReservation Reservation
    | Logout
    | ReceivePaymentStatus Int (Result Http.Error PaymentStatus)


type alias Model =
    { tickets : List FareContract
    , tokens : List Token
    , tokenPayloads : List ( String, String )
    , travelCardId : String
    , inspection : Status Inspection
    , currentTime : Time.Posix
    , expanded : Maybe String
    , reservations : List ( Reservation, ReservationStatus )
    , timeZone : Time.Zone
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
      , timeZone = Time.utc
      }
    , Task.perform AdjustTimeZone Time.here
    )


update : Msg -> Environment -> Model -> PageUpdater Model Msg
update msg env model =
    case msg of
        OnEnterPage ->
            PageUpdater.init model
                |> (Nothing
                        |> GA.SetTitle
                        |> PageUpdater.addGlobalAction
                   )

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
                                |> List.filter (\( reservation, _ ) -> not <| List.member reservation.orderId orderIds)
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
            let
                existsFromBefore =
                    model.reservations
                        |> List.map (Tuple.first >> .paymentId)
                        |> List.member active.paymentId
            in
                if existsFromBefore then
                    PageUpdater.init model

                else
                    PageUpdater.fromPair
                        ( { model | reservations = ( active, NotCaptured ) :: model.reservations }
                        , fetchPaymentStatus env active.paymentId
                        )

        Logout ->
            PageUpdater.init model
                |> PageUpdater.addGlobalAction GA.Logout

        AdjustTimeZone zone ->
            PageUpdater.init { model | timeZone = zone }

        ReceivePaymentStatus paymentId result ->
            let
                updateReservationToStatus state =
                    model.reservations
                        |> List.map
                            (\( reservation, status ) ->
                                if reservation.paymentId /= paymentId then
                                    ( reservation, status )

                                else
                                    ( reservation, state )
                            )

                withoutCurrentReservation =
                    List.filter (Tuple.first >> .paymentId >> (/=) paymentId) model.reservations
            in
                case result of
                    Ok paymentStatus ->
                        case paymentStatus.status of
                            "CAPTURE" ->
                                PageUpdater.init { model | reservations = updateReservationToStatus Captured }

                            "CANCEL" ->
                                PageUpdater.init
                                    { model | reservations = withoutCurrentReservation }

                            "CREDIT" ->
                                PageUpdater.init
                                    { model | reservations = withoutCurrentReservation }

                            "REJECT" ->
                                PageUpdater.init
                                    { model | reservations = withoutCurrentReservation }

                            _ ->
                                PageUpdater.fromPair ( model, fetchPaymentStatus env paymentId )

                    _ ->
                        -- Either there was no longer a reservation, or the payment failed. We treat this
                        -- as if the payment was cancelled so the user can try again.
                        PageUpdater.init
                            { model | reservations = withoutCurrentReservation }


view : Environment -> AppInfo -> Shared -> Model -> Maybe Route -> Html Msg
view env _ shared model _ =
    H.div []
        [ PH.init
            |> PH.setTitle (Just "Mine billetter")
            |> PH.view
        , H.div [ A.class "page page--overview" ]
            [ viewSidebar shared model
            , viewMain shared model
            ]
        ]


viewSidebar : Shared -> Model -> Html Msg
viewSidebar shared model =
    H.div [ A.class "sidebar" ]
        [ viewAccountInfo shared model
        , viewActions shared
        ]


viewAccountInfo : Shared -> Model -> Html Msg
viewAccountInfo shared _ =
    Ui.Section.init
        |> Ui.Section.setMarginBottom True
        |> Ui.Section.viewWithOptions
            [ Ui.Section.viewPaddedItem
                [ Ui.Heading.component "Min profil"
                , Html.Extra.viewMaybe
                    (\d -> H.p [ A.class "accountInfo__item" ] [ H.text <| Util.PhoneNumber.format d.phone ])
                    shared.profile
                , shared.profile
                    |> Util.Maybe.flatMap .travelCard
                    |> Maybe.map .id
                    |> Html.Extra.viewMaybe
                        (\id ->
                            H.p [ A.class "accountInfo__item", A.title "t:kort-nummer" ]
                                [ Icon.travelCard
                                , Ui.TravelCardText.view id
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


viewActions : Shared -> Html Msg
viewActions shared =
    Ui.Section.view
        [ B.init "Kjøp ny periodebillett"
            |> B.setDisabled False
            |> B.setIcon (Just Icon.ticketAdd)
            |> B.setAttributes [ Route.href Route.Shop ]
            |> B.setElement H.a
            |> B.primary B.Primary_2
            |> Html.Extra.viewIf (Shared.hasPeriodTickets shared)
        , B.init "Kjøp nytt klippekort"
            |> B.setDisabled False
            |> B.setElement H.a
            |> B.setIcon (Just Icon.tickets)
            |> B.setAttributes [ Route.href Route.ShopCarnet ]
            |> B.primary B.Primary_2
            |> Html.Extra.viewIf (Shared.hasCarnetTickets shared)
        ]


viewMain : Shared -> Model -> Html Msg
viewMain shared model =
    let
        validTickets =
            Util.FareContract.filterValidNow model.currentTime model.tickets

        infoMessage =
            Ui.Section.init
                |> Ui.Section.setMarginBottom True
                |> Ui.Section.viewWithOptions
                    [ H.p []
                        [ H.text "Denne billettvisningen er ikke gyldig ved eventuell billettkontroll."
                        , H.br [] []
                        , H.text "På reise vil det t:kortet du har registrert på din profil være gyldig billettbevis."
                        ]
                        |> Message.Info
                        |> Message.message
                    ]
    in
        H.div [ A.class "main" ]
            [ if List.isEmpty validTickets && List.isEmpty model.reservations then
                H.div [ A.class "pageOverview__empty" ]
                    [ H.img [ A.src "/images/empty-illustration.svg", A.alt "" ] []
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
                    , timeZone = model.timeZone
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


fetchPaymentStatus : Environment -> Int -> Cmd Msg
fetchPaymentStatus env paymentId =
    Process.sleep 500
        |> Task.andThen
            (\_ ->
                TicketService.getPaymentStatus env paymentId
                    |> Http.toTask
            )
        |> Task.attempt (ReceivePaymentStatus paymentId)
