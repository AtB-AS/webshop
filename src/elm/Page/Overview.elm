module Page.Overview exposing (Model, Msg(..), init, subscriptions, update, view)

import Base exposing (AppInfo)
import Data.FareContract exposing (FareContract, FareContractState(..), TravelRight(..))
import Data.Reservation exposing (Reservation)
import Data.Webshop exposing (Inspection, Token)
import Environment exposing (Environment)
import Fragment.Icon as Icon
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Extra
import Http
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Notification
import PageUpdater exposing (PageUpdater)
import Route exposing (Route)
import Service.Misc as MiscService exposing (Profile)
import Service.Ticket as TicketService
import Shared exposing (Shared)
import Task
import Time
import Ui.Button as B
import Ui.ImgThemed
import Ui.Message as Message
import Ui.PageHeader as PH
import Ui.Section as S
import Ui.TicketDetails
import Ui.TravelCardText
import Util.FareContract
import Util.Maybe
import Util.PhoneNumber
import Util.Reservation
import Util.Status exposing (Status(..))


type Msg
    = OnEnterPage
    | ReceiveFareContracts (Result Decode.Error (List FareContract))
    | ReceiveReservations (Result Decode.Error (List Reservation))
    | ReceiveTokenPayloads (Result Decode.Error (List ( String, String )))
    | OpenEditTravelCard
    | UpdateTime Time.Posix
    | AdjustTimeZone Time.Zone
    | ToggleTicket String
    | Logout
    | RequestReceipt String
    | ReceiveReceipt String (Result Http.Error ())


type alias Model =
    { validFareContracts : List FareContract
    , validReservations : List Reservation
    , allFareContracts : List FareContract
    , allReservations : List Reservation
    , tokens : List Token
    , tokenPayloads : List ( String, String )
    , travelCardId : String
    , inspection : Status Inspection
    , currentTime : Time.Posix
    , expanded : Maybe String
    , timeZone : Time.Zone
    , error : Maybe String
    , sendingReceipt : List String
    }


init : ( Model, Cmd Msg )
init =
    ( { validFareContracts = []
      , validReservations = []
      , allFareContracts = []
      , allReservations = []
      , tokens = []
      , tokenPayloads = []
      , travelCardId = ""
      , inspection = NotLoaded
      , currentTime = Time.millisToPosix 0
      , expanded = Nothing
      , timeZone = Time.utc
      , error = Nothing
      , sendingReceipt = []
      }
    , Task.perform AdjustTimeZone Time.here
    )


update : Msg -> Environment -> Model -> Shared -> PageUpdater Model Msg
update msg env model shared =
    case msg of
        OnEnterPage ->
            PageUpdater.init model
                |> (PageUpdater.addGlobalAction <| GA.SetTitle <| Nothing)
                |> (PageUpdater.addGlobalAction <| GA.FocusItem <| Just "page-header")

        ReceiveFareContracts result ->
            case result of
                Ok fareContracts ->
                    PageUpdater.init
                        { model
                            | allFareContracts = fareContracts
                            , validFareContracts =
                                Util.FareContract.toSortedValidFareContracts model.currentTime fareContracts
                            , validReservations =
                                Util.Reservation.filterValidReservations model.currentTime fareContracts model.allReservations
                            , error = Nothing
                        }

                Err _ ->
                    PageUpdater.init
                        { model
                            | error =
                                Just "Fikk ikke hentet billetter. Prøv igjen senere, eller ta kontakt med kundeservice om problemet vedvarer."
                        }

        ReceiveReservations result ->
            case result of
                Ok reservations ->
                    PageUpdater.init
                        { model
                            | allReservations = reservations
                            , validReservations =
                                Util.Reservation.filterValidReservations model.currentTime model.allFareContracts reservations
                        }

                Err _ ->
                    PageUpdater.init
                        { model
                            | error =
                                Just "Fikk ikke hentet billetter. Prøv igjen senere, eller ta kontakt med kundeservice om problemet vedvarer."
                        }

        ReceiveTokenPayloads result ->
            case result of
                Ok value ->
                    PageUpdater.init { model | tokenPayloads = value }

                Err _ ->
                    PageUpdater.init { model | tokenPayloads = [] }

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

        Logout ->
            PageUpdater.init model
                |> PageUpdater.addGlobalAction GA.Logout

        AdjustTimeZone zone ->
            PageUpdater.init { model | timeZone = zone }

        RequestReceipt orderId ->
            case shared.profile of
                Just profile ->
                    PageUpdater.fromPair
                        ( { model | sendingReceipt = orderId :: model.sendingReceipt }
                        , sendReceipt env profile orderId
                        )

                Nothing ->
                    -- TODO: Handle no profile and no email in profile
                    PageUpdater.init model

        ReceiveReceipt orderId result ->
            let
                message =
                    case result of
                        Ok () ->
                            H.text "Kvitteringen ble sendt til din e-post."
                                |> Message.Valid

                        Err _ ->
                            H.text "Kunne ikke sende kvittering. Ta kontakt med kundeservice og oppgi ordre ID."
                                |> Message.Error
            in
                PageUpdater.init { model | sendingReceipt = List.Extra.remove orderId model.sendingReceipt }
                    |> PageUpdater.addGlobalAction
                        (message
                            |> Message.message
                            |> (\s -> Notification.setContent s Notification.init)
                            |> GA.ShowNotification
                        )


view : Environment -> AppInfo -> Shared -> Model -> Maybe Route -> Html Msg
view env _ shared model _ =
    H.div []
        [ PH.init
            |> PH.setTitle (Just "Mine billetter")
            |> PH.view
        , H.div [ A.class "page page--overview" ]
            [ viewSidebar env shared model
            , viewMain shared model
            ]
        ]


viewSidebar : Environment -> Shared -> Model -> Html Msg
viewSidebar env shared model =
    H.aside [ A.class "sidebar" ]
        [ viewAccountInfo env shared model
        , viewActions shared
        ]


viewAccountInfo : Environment -> Shared -> Model -> Html Msg
viewAccountInfo env shared _ =
    let
        name =
            viewNameMaybe shared.profile

        phone =
            viewPhoneMaybe shared.profile
    in
        S.init
            |> S.setMarginBottom True
            |> S.viewWithOptions
                [ S.viewLabelItem "Min profil"
                    [ Html.Extra.viewMaybe identity name
                    , Html.Extra.viewMaybe identity phone
                    , if phone == Nothing && name == Nothing then
                        H.p [] [ H.text "Ingen informasjon satt" ]

                      else
                        Html.Extra.nothing
                    ]
                , shared.profile
                    |> Util.Maybe.flatMap .travelCard
                    |> Maybe.map .id
                    |> Html.Extra.viewMaybe
                        (\id ->
                            S.viewLabelItem "T:kort"
                                [ H.p [ A.class "accountInfo__item", A.title "t:kort-nummer" ]
                                    [ Icon.travelCard
                                    , Ui.TravelCardText.view id
                                    ]
                                ]
                        )
                , S.viewLabelItem
                    "Kundenummer"
                    [ H.p [ A.class "accountInfo__item", A.title "Kundenummer" ]
                        [ Icon.profile
                        , H.text <| Util.Maybe.mapWithDefault String.fromInt "Fant ikke kundenummer. Ta kontakt med kundeservice." env.customerNumber
                        ]
                    ]
                , B.init "Rediger profil"
                    |> B.setDisabled False
                    |> B.setIcon (Just Icon.edit)
                    |> B.setAttributes [ Route.href Route.Settings ]
                    |> B.setElement H.a
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


viewNameMaybe : Maybe Profile -> Maybe (Html msg)
viewNameMaybe maybeProfile =
    case maybeProfile of
        Just { firstName, lastName } ->
            if hasField firstName || hasField lastName then
                Just <| H.p [] [ H.text <| firstName ++ " " ++ lastName ]

            else
                Nothing

        _ ->
            Nothing


viewPhoneMaybe : Maybe Profile -> Maybe (Html msg)
viewPhoneMaybe maybeProfile =
    case maybeProfile of
        Just { phone } ->
            if hasField phone then
                Just <| H.p [] [ H.text <| Util.PhoneNumber.format phone ]

            else
                Nothing

        _ ->
            Nothing


hasField : String -> Bool
hasField x =
    x /= "" && x /= "_"


viewActions : Shared -> Html Msg
viewActions shared =
    S.view
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
        validFareContracts =
            Util.FareContract.toSortedValidFareContracts model.currentTime model.validFareContracts

        validReservations =
            Util.Reservation.filterValidReservations model.currentTime model.allFareContracts model.validReservations

        emptyResults =
            List.isEmpty validFareContracts && List.isEmpty validReservations
    in
        H.div [ A.class "main" ]
            [ case ( emptyResults, model.error ) of
                ( True, Nothing ) ->
                    H.div [ A.class "pageOverview__empty" ]
                        [ Ui.ImgThemed.view [ A.src "/assets/colors/images/empty-illustration.svg", A.alt "" ] []
                        , H.text "Ingen billetter er tilknyttet din profil."
                        ]

                ( _, Just error ) ->
                    Message.error error

                ( False, Nothing ) ->
                    H.div [] (viewPending validReservations ++ viewTicketCards shared validFareContracts model)
            ]


viewPending : List Reservation -> List (Html msg)
viewPending reservations =
    List.map Ui.TicketDetails.viewReservation reservations


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
                    (if List.member f.orderId model.sendingReceipt then
                        Nothing

                     else
                        Just (RequestReceipt f.orderId)
                    )
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ MiscService.receiveTokens (Decode.decodeValue tokenPayloadsDecoder >> ReceiveTokenPayloads)
        , MiscService.receiveFareContracts
            (Decode.decodeValue (Decode.list MiscService.fareContractDecoder)
                >> ReceiveFareContracts
            )
        , MiscService.receiveReservations
            (Decode.decodeValue (Decode.list MiscService.reservationDecoder)
                >> ReceiveReservations
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


sendReceipt : Environment -> Profile -> String -> Cmd Msg
sendReceipt env profile orderId =
    if String.isEmpty profile.email then
        Cmd.none

    else
        TicketService.receipt env profile.email orderId
            |> Http.toTask
            |> Task.attempt (ReceiveReceipt orderId)
