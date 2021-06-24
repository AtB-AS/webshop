module Page.Shop.Period exposing (Model, Msg(..), init, subscriptions, update, view)

import Base exposing (AppInfo)
import Data.RefData exposing (FareProduct, LangString(..), ProductType(..), UserType(..))
import Data.Ticket exposing (Offer, PaymentType(..), Reservation)
import Environment exposing (Environment)
import Fragment.Icon as Icon
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Html.Extra
import Http
import Page.Shop.CommonViews as Common
import Page.Shop.Summary as SummaryPage
import Page.Shop.Utils as Utils exposing (TravelDateTime(..))
import PageUpdater exposing (PageUpdater)
import Process
import Route exposing (Route)
import Service.Misc as MiscService
import Service.Ticket as TicketService
import Set
import Shared exposing (Shared)
import Task
import Time exposing (Posix)
import Ui.Group
import Ui.Input.Radio as Radio
import Ui.Input.Text as Text
import Ui.PageHeader as PH
import Ui.Section as Section
import Util.Func as Func
import Util.Status exposing (Status(..))
import Util.Task as TaskUtil
import Util.Time as TimeUtil


type Msg
    = OnEnterPage
    | OnLeavePage
    | ResetState
    | FetchOffers
    | ReceiveOffers (Result Http.Error (List Offer))
    | CloseShop
    | SetProduct String Bool
    | SetFromZone String
    | SetToZone String
    | SetUser UserType Bool
    | ShowView MainView
    | SetTime String
    | SetDate String
    | UpdateNow Time.Posix
    | UpdateZone Time.Zone
    | GetIsoTime String
    | SetTravelDateTime TravelDateTime
    | CloseSummary
    | GoToSummary
    | SummarySubMsg SummaryPage.Msg


type MainView
    = Travelers
    | Duration
    | Start
    | Zones
    | None


type alias Model =
    { product : Maybe String
    , fromZone : Maybe String
    , toZone : Maybe String
    , users : List ( UserType, Int )
    , offers : Status (List Offer)
    , reservation : Status Reservation
    , mainView : MainView
    , travelDateTime : TravelDateTime
    , now : Posix
    , inputTravelDate : String
    , inputTravelTime : String
    , timeZone : Time.Zone
    , summary : Maybe SummaryPage.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { product = Nothing
      , fromZone = Nothing
      , toZone = Nothing
      , users = [ ( UserTypeAdult, 1 ) ]
      , offers = NotLoaded
      , reservation = NotLoaded
      , mainView = Duration
      , travelDateTime = TravelNow
      , now = Time.millisToPosix 0
      , inputTravelDate = ""
      , inputTravelTime = ""
      , timeZone = Time.utc
      , summary = Nothing
      }
    , Cmd.batch
        [ TaskUtil.doTask FetchOffers
        , Task.perform UpdateZone Time.here
        ]
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

        SetProduct product _ ->
            PageUpdater.fromPair
                ( { model | product = Just product, users = maybeResetUsers shared product model.users }
                , TaskUtil.doTask FetchOffers
                )

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

        SetTravelDateTime (TravelFuture maybeFuture) ->
            case maybeFuture of
                Nothing ->
                    let
                        travelDate =
                            if String.isEmpty model.inputTravelDate then
                                TimeUtil.toIsoDate model.timeZone model.now

                            else
                                model.inputTravelDate

                        travelTime =
                            if String.isEmpty model.inputTravelTime then
                                TimeUtil.toHoursAndMinutes model.timeZone <| TimeUtil.addHours 1 model.now

                            else
                                model.inputTravelTime
                    in
                        PageUpdater.fromPair
                            ( { model
                                | travelDateTime = TravelFuture Nothing
                                , inputTravelDate = travelDate
                                , inputTravelTime = travelTime
                              }
                            , MiscService.convertTime ( travelDate, travelTime )
                            )

                future ->
                    PageUpdater.fromPair
                        ( { model | travelDateTime = TravelFuture <| future }
                        , TaskUtil.doTask FetchOffers
                        )

        SetTravelDateTime TravelNow ->
            PageUpdater.fromPair
                ( { model | travelDateTime = TravelNow }
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
                            |> List.filter (.type_ >> (==) ProductTypePeriod)

                    ( firstZone, defaultProduct ) =
                        Utils.defaultDerivedData shared availableProducts

                    dataNotLoadedYet =
                        List.isEmpty shared.availableFareProducts && List.isEmpty shared.tariffZones

                    newProduct =
                        Maybe.withDefault defaultProduct model.product

                    newFromZone =
                        Maybe.withDefault firstZone model.fromZone

                    newToZone =
                        Maybe.withDefault firstZone model.toZone

                    travelTime =
                        case model.travelDateTime of
                            TravelFuture (Just time) ->
                                Just time

                            _ ->
                                Nothing
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
                                travelTime
                            )

        ReceiveOffers result ->
            case result of
                Ok offers ->
                    PageUpdater.init { model | offers = Loaded offers }

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

        SetDate date ->
            PageUpdater.fromPair
                ( { model | inputTravelDate = date }
                , MiscService.convertTime ( date, model.inputTravelTime )
                )

        SetTime time ->
            PageUpdater.fromPair
                ( { model | inputTravelTime = time }
                , MiscService.convertTime ( model.inputTravelDate, time )
                )

        UpdateNow now ->
            PageUpdater.init { model | now = now }

        UpdateZone zone ->
            PageUpdater.init { model | timeZone = zone }

        GetIsoTime isoTime ->
            PageUpdater.fromPair
                ( model
                , TaskUtil.doTask <| SetTravelDateTime <| TravelFuture (Just isoTime)
                )

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


maybeResetUsers : Shared -> String -> List ( UserType, Int ) -> List ( UserType, Int )
maybeResetUsers shared product users =
    let
        data =
            users
                |> List.filter (Tuple.first >> Func.flip List.member (Utils.findLimitations product shared.productLimitations))
    in
        if List.isEmpty data then
            [ ( UserTypeAdult, 1 ) ]

        else
            data


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
                |> List.filter (.type_ >> (==) ProductTypePeriod)

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
                        |> PH.setBackButton (Just ( "Tilbake", E.onClick CloseSummary ))
                        |> PH.view
                    , SummaryPage.view shared summaryModel
                        |> H.map SummarySubMsg
                    ]

            _ ->
                H.div []
                    [ PH.init
                        |> PH.setTitle (Just "Kjøp nytt periodebillett")
                        |> PH.setBackButton (Just ( "Avbryt", E.onClick CloseShop ))
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
                                { title = "Billettype"
                                , icon = Icon.duration
                                , value = summary.duration
                                , open = model.mainView == Duration
                                , readonly = False
                                , onOpenClick = Just (ShowView Duration)
                                , id = "varighet"
                                , editTextSuffix = "varighet"
                                }
                                [ viewProducts model defaultProduct shared.availableFareProducts ]
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
                            , Ui.Group.view
                                { title = "Gyldig fra og med"
                                , icon = Icon.ticket
                                , value = summary.start
                                , open = model.mainView == Start
                                , readonly = False
                                , onOpenClick = Just (ShowView Start)
                                , id = "duration"
                                , editTextSuffix = "tid"
                                }
                                (viewStart model)
                            , Ui.Group.view
                                { title = "Soner"
                                , icon = Icon.map
                                , value = summary.zones
                                , open = model.mainView == Zones
                                , readonly = False
                                , onOpenClick = Just (ShowView Zones)
                                , id = "zones"
                                , editTextSuffix = "sone"
                                }
                                [ Common.viewZones model defaultZone shared.tariffZones SetFromZone SetToZone ]
                            ]
                        , Common.viewSummary shared model disableButtons GoToSummary
                        ]
                    ]


langString : LangString -> String
langString (LangString _ value) =
    value


viewStart : Model -> List (Html Msg)
viewStart model =
    let
        isFutureSelected =
            model.travelDateTime /= TravelNow
    in
        [ Radio.viewGroup "Velg avreisetid"
            [ Radio.init "travel-now"
                |> Radio.setTitle "Kjøpstidspunkt"
                |> Radio.setName "traveltime"
                |> Radio.setChecked (not isFutureSelected)
                |> Radio.setOnCheck (Just <| \_ -> SetTravelDateTime TravelNow)
                |> Radio.view
            , Radio.init "travel-future"
                |> Radio.setTitle "Velg dato og tid"
                |> Radio.setName "traveltime"
                |> Radio.setChecked isFutureSelected
                |> Radio.setOnCheck (Just <| \_ -> SetTravelDateTime <| TravelFuture Nothing)
                |> Radio.view
            ]
        , Html.Extra.viewIf isFutureSelected <|
            Section.viewHorizontalGroup
                [ Text.init "date"
                    |> Text.setTitle (Just "Dato")
                    |> Text.setOnInput (Just SetDate)
                    |> Text.setAttributes [ A.min <| TimeUtil.toIsoDate model.timeZone model.now ]
                    |> Text.setType "date"
                    |> Text.setValue (Just model.inputTravelDate)
                    |> Text.view
                , Text.init "time"
                    |> Text.setTitle (Just "Tid")
                    |> Text.setOnInput (Just SetTime)
                    |> Text.setAttributes
                        [ A.min <| TimeUtil.toHoursAndMinutes model.timeZone model.now ]
                    |> Text.setType "time"
                    |> Text.setValue (Just model.inputTravelTime)
                    |> Text.view
                ]
        ]


viewProducts : Model -> String -> List FareProduct -> Html Msg
viewProducts model defaultProduct products =
    products
        |> List.map (viewProduct model defaultProduct)
        |> Radio.viewGroup "Velg billettype"


viewProduct : Model -> String -> FareProduct -> Html Msg
viewProduct model defaultProduct product =
    let
        selectedProduct =
            Maybe.withDefault defaultProduct model.product

        isCurrent =
            selectedProduct == product.id
    in
        Radio.init product.id
            |> Radio.setTitle (langString product.name)
            |> Radio.setName "product"
            |> Radio.setChecked isCurrent
            |> Radio.setOnCheck (Just <| SetProduct product.id)
            |> Radio.view


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ MiscService.convertedTime GetIsoTime
        , Time.every 1000 UpdateNow
        , SummaryPage.subscriptions |> Sub.map SummarySubMsg
        ]



-- INTERNAL


fetchOffers : Environment -> String -> String -> String -> List ( UserType, Int ) -> Maybe String -> Cmd Msg
fetchOffers env product fromZone toZone users travelDate =
    [ fromZone, toZone ]
        |> Set.fromList
        |> Set.toList
        |> TicketService.search env travelDate product users
        |> Http.toTask
        |> Task.attempt ReceiveOffers
