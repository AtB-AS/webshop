module Page.Shop.Carnet exposing (Model, Msg(..), init, subscriptions, update, view)

import Base exposing (AppInfo)
import Data.RefData exposing (LangString(..), ProductType(..), UserType(..))
import Data.Ticket exposing (Offer, PaymentType(..), Reservation)
import Environment exposing (Environment)
import Fragment.Icon as Icon
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
import Http
import Page.Shop.CommonViews as Common
import Page.Shop.Summary as SummaryPage
import Page.Shop.Utils as Utils exposing (TravelDateTime(..))
import PageUpdater exposing (PageUpdater)
import Process
import Route exposing (Route)
import Service.Ticket as TicketService
import Set
import Shared exposing (Shared)
import Task
import Time
import Ui.Group
import Ui.PageHeader as PH
import Ui.Section as Section
import Util.Status exposing (Status(..))
import Util.Task as TaskUtil


type Msg
    = OnEnterPage
    | OnLeavePage
    | ResetState
    | FetchOffers
    | ReceiveOffers (Result Http.Error (List Offer))
    | CloseShop
    | SetFromZone String
    | SetToZone String
    | SetUser UserType Bool
    | ShowView MainView
    | CloseSummary
    | GoToSummary
    | SummarySubMsg SummaryPage.Msg


type MainView
    = Travelers
    | None


type alias Model =
    { product : Maybe String
    , fromZone : Maybe String
    , toZone : Maybe String
    , users : List ( UserType, Int )
    , offers : Status (List Offer)
    , reservation : Status Reservation
    , mainView : MainView
    , summary : Maybe SummaryPage.Model
    , timeZone : Time.Zone
    , travelDateTime : TravelDateTime
    , travelDateTimeEnd : TravelDateTime
    }


init : ( Model, Cmd Msg )
init =
    ( { product = Nothing
      , fromZone = Nothing
      , toZone = Nothing
      , users = [ ( UserTypeAdult, 1 ) ]
      , offers = NotLoaded
      , reservation = NotLoaded
      , mainView = Travelers
      , summary = Nothing
      , travelDateTime = TravelNow
      , travelDateTimeEnd = TravelFuture Nothing
      , timeZone = Time.utc
      }
    , TaskUtil.doTask FetchOffers
    )


update : Msg -> Environment -> Model -> Shared -> PageUpdater Model Msg
update msg env model shared =
    case msg of
        ResetState ->
            PageUpdater.init <| Tuple.first init

        OnEnterPage ->
            PageUpdater.fromPair ( model, Tuple.second init )
                |> (Just "Kjøp billett"
                        |> GA.SetTitle
                        |> PageUpdater.addGlobalAction
                   )

        OnLeavePage ->
            PageUpdater.init { model | summary = Nothing }

        SetFromZone zone ->
            PageUpdater.fromPair
                ( { model | fromZone = Just zone }
                , TaskUtil.doTask FetchOffers
                )

        SetToZone zone ->
            PageUpdater.fromPair
                ( { model | toZone = Just zone }
                , TaskUtil.doTask FetchOffers
                )

        SetUser userType _ ->
            PageUpdater.fromPair
                ( { model | users = [ ( userType, 1 ) ] }
                , TaskUtil.doTask FetchOffers
                )

        FetchOffers ->
            if List.isEmpty model.users then
                PageUpdater.init model

            else
                let
                    oldOffers =
                        case model.offers of
                            Loaded offers ->
                                Just offers

                            Loading offers ->
                                offers

                            _ ->
                                Nothing

                    availableProducts =
                        shared.fareProducts
                            |> List.filter (.type_ >> (==) ProductTypeCarnet)

                    ( firstZone, defaultProduct ) =
                        Utils.defaultDerivedData shared availableProducts

                    dataNotLoadedYet =
                        List.isEmpty availableProducts && List.isEmpty shared.tariffZones

                    newProduct =
                        Maybe.withDefault defaultProduct model.product

                    newFromZone =
                        Maybe.withDefault firstZone model.fromZone

                    newToZone =
                        Maybe.withDefault firstZone model.toZone
                in
                    if dataNotLoadedYet then
                        PageUpdater.fromPair
                            ( model
                            , Process.sleep 500 |> Task.attempt (\_ -> FetchOffers)
                            )

                    else
                        PageUpdater.fromPair
                            ( { model
                                | offers = Loading oldOffers
                                , reservation = NotLoaded
                                , product = Just newProduct
                                , fromZone = Just newFromZone
                                , toZone = Just newToZone
                              }
                            , fetchOffers env
                                newProduct
                                newFromZone
                                newToZone
                                model.users
                                Nothing
                            )

        ReceiveOffers result ->
            case result of
                Ok offers ->
                    PageUpdater.init
                        { model
                            | offers = Loaded offers
                            , travelDateTimeEnd = TravelFuture Nothing
                        }

                Err _ ->
                    let
                        errorMessage =
                            "Kunne ikke laste inn billettinformasjon. Prøv igjen."
                    in
                        PageUpdater.init { model | offers = Failed errorMessage }

        CloseShop ->
            PageUpdater.fromPair ( model, TaskUtil.doTask ResetState )
                |> PageUpdater.addGlobalAction (GA.RouteTo Route.Home)

        ShowView mainView ->
            PageUpdater.init (toggleShowMainView model mainView)

        CloseSummary ->
            PageUpdater.init { model | summary = Nothing }

        GoToSummary ->
            case model.offers of
                Loaded offers ->
                    PageUpdater.init
                        { model
                            | summary =
                                Just <|
                                    SummaryPage.init
                                        { productId = Maybe.withDefault "" model.product
                                        , fromZoneId = Maybe.withDefault "" model.fromZone
                                        , toZoneId = Maybe.withDefault "" model.toZone
                                        , travelDate = model.travelDateTime
                                        , travelDateEnd = model.travelDateTimeEnd
                                        , timeZone = model.timeZone
                                        }
                                        offers
                        }

                _ ->
                    PageUpdater.init model

        SummarySubMsg subMsg ->
            case model.summary of
                Nothing ->
                    PageUpdater.init model

                Just summary ->
                    SummaryPage.update subMsg env summary shared
                        |> PageUpdater.map (\newModel -> { model | summary = Just newModel }) SummarySubMsg


toggleShowMainView : Model -> MainView -> Model
toggleShowMainView model mainView =
    { model
        | mainView =
            if model.mainView == mainView then
                None

            else
                mainView
    }


view : Environment -> AppInfo -> Shared -> Model -> Maybe Route -> Html Msg
view _ _ shared model _ =
    let
        availableProducts =
            shared.fareProducts
                |> List.filter (.type_ >> (==) ProductTypeCarnet)

        ( defaultZone, defaultProduct ) =
            Utils.defaultDerivedData shared availableProducts

        summary =
            Utils.modelSummary ( defaultZone, defaultProduct ) shared model

        errorMessage =
            case model.offers of
                Failed message ->
                    Just message

                _ ->
                    Nothing

        emptyOffers =
            case model.offers of
                Loaded offers ->
                    List.isEmpty offers

                _ ->
                    True

        disableButtons =
            (errorMessage /= Nothing)
                || emptyOffers
                || (case model.reservation of
                        Loading _ ->
                            True

                        Loaded _ ->
                            True

                        _ ->
                            False
                   )
    in
        case model.summary of
            Just summaryModel ->
                H.div []
                    [ PH.init
                        |> PH.setTitle (Just "Oppsummering")
                        |> PH.setBackButton ( "Tilbake", CloseSummary )
                        |> PH.view
                    , SummaryPage.view shared summaryModel
                        |> H.map SummarySubMsg
                    ]

            _ ->
                H.div []
                    [ PH.init
                        |> PH.setTitle (Just "Kjøp nytt klippekort")
                        |> PH.setBackButton ( "Avbryt", CloseShop )
                        |> PH.setBackIcon Icon.cross
                        |> PH.view
                    , H.div [ A.class "page" ]
                        [ Section.view
                            [ Ui.Group.view
                                { title = "Reisetype"
                                , icon = Icon.bus
                                , value = Just "Buss og trikk"
                                , open = False
                                , readonly = True
                                , onOpenClick = Nothing
                                , id = "reisetype"
                                , editTextSuffix = "billett"
                                }
                                []
                            , Ui.Group.view
                                { title = "Antall billetter"
                                , icon = Icon.tickets
                                , value = summary.product
                                , open = False
                                , readonly = True
                                , onOpenClick = Nothing
                                , id = "product"
                                , editTextSuffix = "antall"
                                }
                                []
                            , Ui.Group.view
                                { title = "Reisende"
                                , icon = Icon.bus
                                , value =
                                    summary.users
                                        |> List.head
                                        |> Maybe.map (\( name, num ) -> String.fromInt num ++ " " ++ name)
                                , open = model.mainView == Travelers
                                , readonly = False
                                , onOpenClick = Just (ShowView Travelers)
                                , id = "reisende"
                                , editTextSuffix = "reisende"
                                }
                                [ Common.viewUserProfiles defaultProduct model SetUser shared ]
                            , Section.viewWithIcon Icon.map
                                [ Common.viewZones model defaultZone shared.tariffZones SetFromZone SetToZone ]
                            ]
                        , H.div []
                            [ Common.viewSummary shared model disableButtons GoToSummary Nothing
                            ]
                        ]
                    ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    SummaryPage.subscriptions
        |> Sub.map SummarySubMsg



-- INTERNAL


fetchOffers : Environment -> String -> String -> String -> List ( UserType, Int ) -> Maybe String -> Cmd Msg
fetchOffers env product fromZone toZone users travelDate =
    [ fromZone, toZone ]
        |> Set.fromList
        |> Set.toList
        |> TicketService.search env travelDate product users
        |> Http.toTask
        |> Task.attempt ReceiveOffers
