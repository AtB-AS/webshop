module Main exposing (main)

import Base exposing (AppInfo)
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Environment exposing (DistributionEnvironment(..), Environment, Language(..))
import Error exposing (Error)
import Fragment.Icon as Icon
import GlobalActions as GA exposing (GlobalAction(..))
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Html.Extra
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodeP
import Notification exposing (Notification)
import Page.Account as AccountPage exposing (Msg(..))
import Page.History as HistoryPage
import Page.Login as LoginPage
import Page.Onboarding as OnboardingPage
import Page.Overview as OverviewPage
import Page.Shop.Carnet as ShopCarnetPage
import Page.Shop.Period as ShopPage
import Page.VerifyUser as VerifyUserPage
import PageUpdater exposing (PageUpdater)
import Route exposing (LoginMethodPath(..), Route(..))
import Service.FirebaseAuth as FirebaseAuth
import Service.Misc as MiscService
import Shared exposing (Shared)
import Task
import Time
import Ui.GlobalNotifications
import Ui.Message
import Ui.PageHeader as PH
import Ui.ScreenReaderText
import Url exposing (Url)
import Util.PageTitle
import Util.Status exposing (Status(..))
import Util.Task as TaskUtil


type Msg
    = GlobalAction (GlobalAction Msg)
    | SetRoute (Maybe Route)
    | RouteTo Route
    | UrlChanged Url
    | UrlRequested Browser.UrlRequest
    | MaybeCloseNotification Time.Posix
    | OverviewMsg OverviewPage.Msg
    | ShopMsg ShopPage.Msg
    | ShopCarnetMsg ShopCarnetPage.Msg
    | HistoryMsg HistoryPage.Msg
    | AccountMsg AccountPage.Msg
    | LoginMsg LoginPage.Msg
    | OnboardingMsg OnboardingPage.Msg
    | VerifyUserMsg VerifyUserPage.Msg
    | SharedMsg Shared.Msg
    | StartOnboarding ( String, String, String )
    | StartVerifyUser String
    | LogOut
    | CloseValidityWarning
    | LoggedInData (Result Decode.Error UserData)
    | LoggedInError (Result Decode.Error AuthError)
    | Focus (Result Dom.Error ())


type alias Model =
    { environment : Environment
    , appInfo : AppInfo
    , overview : OverviewPage.Model
    , shop : ShopPage.Model
    , shopCarnet : ShopCarnetPage.Model
    , history : HistoryPage.Model
    , account : AccountPage.Model
    , shared : Shared
    , login : LoginPage.Model
    , onboarding : Maybe OnboardingPage.Model
    , verifyUser : Maybe VerifyUserPage.Model
    , route : Maybe Route.Route
    , errors : List Error
    , notifications : List (Notification Msg)
    , userData : Status UserData
    , authError : AuthError
    , navKey : Nav.Key
    }


type alias Flags =
    { isDevelopment : Bool
    , localUrl : String
    , baseUrl : String
    , ticketUrl : String
    , refDataUrl : String
    , version : String
    , commit : String
    , installId : String
    , loggedIn : Bool
    , showValidityWarning : Bool
    }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute =
    setRouteInternal False


setInitialRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setInitialRoute =
    setRouteInternal True


setRouteInternal : Bool -> Maybe Route -> Model -> ( Model, Cmd Msg )
setRouteInternal initialRoute maybeRoute model =
    let
        moveAwayCmd =
            case ( maybeRoute, model.route ) of
                -- If navigating away from Settings, reset all state.
                ( Just _, Just Route.Settings ) ->
                    TaskUtil.doTask <| AccountMsg AccountPage.ResetState

                ( Just _, Just Route.Shop ) ->
                    TaskUtil.doTask <| ShopMsg ShopPage.OnLeavePage

                ( Just _, Just Route.ShopCarnet ) ->
                    TaskUtil.doTask <| ShopCarnetMsg ShopCarnetPage.OnLeavePage

                _ ->
                    Cmd.none
    in
        ( { model | route = maybeRoute }
        , if maybeRoute == model.route && not initialRoute then
            Cmd.none

          else
            Cmd.batch
                [ moveAwayCmd
                , case maybeRoute of
                    Just Route.Shop ->
                        TaskUtil.doTask <| ShopMsg ShopPage.OnEnterPage

                    Just (Route.Login loginPath) ->
                        TaskUtil.doTask <| LoginMsg <| LoginPage.OnEnterPage loginPath

                    Just Route.ShopCarnet ->
                        TaskUtil.doTask <| ShopCarnetMsg ShopCarnetPage.OnEnterPage

                    Just Route.Settings ->
                        TaskUtil.doTask <| AccountMsg AccountPage.OnEnterPage

                    Just Route.History ->
                        TaskUtil.doTask <| HistoryMsg HistoryPage.OnEnterPage

                    Just Route.Home ->
                        TaskUtil.doTask <| OverviewMsg OverviewPage.OnEnterPage

                    Just (Route.Payment maybeReservation) ->
                        let
                            noPaymentId =
                                maybeReservation.paymentId == Nothing

                            isCancelled =
                                maybeReservation.responseCode
                                    |> Maybe.map ((==) Route.Cancel)
                                    |> Maybe.withDefault False

                            reservation =
                                { transactionId = Maybe.withDefault 1 maybeReservation.transactionId
                                , paymentId = Maybe.withDefault 1 maybeReservation.paymentId
                                , orderId = Maybe.withDefault "" maybeReservation.orderId
                                , url = ""
                                }
                        in
                            if noPaymentId || isCancelled then
                                Route.modifyUrl model.navKey Route.Home

                            else
                                Cmd.batch
                                    [ TaskUtil.doTask <| OverviewMsg <| OverviewPage.AddActiveReservation reservation
                                    , Route.modifyUrl model.navKey Route.Home
                                    ]

                    Just _ ->
                        TaskUtil.doTask <| GlobalAction <| GA.SetTitle Nothing

                    Nothing ->
                        TaskUtil.doTask <| RouteTo Route.Home
                ]
        )


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        distributionEnv =
            if flags.isDevelopment then
                Development

            else
                Production

        environment : Environment
        environment =
            { distributionEnv = distributionEnv
            , localUrl = flags.localUrl
            , baseUrl = flags.baseUrl
            , ticketUrl = flags.ticketUrl
            , refDataUrl = flags.refDataUrl
            , language = English
            , installId = flags.installId
            , showValidityWarning = flags.showValidityWarning
            , customerId = Nothing
            , customerNumber = 0
            , customerEmail = ""
            , token = ""
            }

        appInfo =
            { title = "AtB Nettbutikk"
            , version = flags.version
            , commit = flags.commit
            }

        ( overviewModel, overviewCmd ) =
            OverviewPage.init

        ( accountModel, accountCmd ) =
            AccountPage.init

        ( loginModel, loginCmd ) =
            LoginPage.init

        route =
            Route.fromUrl url

        userData =
            if flags.loggedIn then
                Loading Nothing

            else
                NotLoaded

        ( routeModel, routeCmd ) =
            setInitialRoute route
                { environment = environment
                , appInfo = appInfo
                , overview = overviewModel
                , shop = Tuple.first ShopPage.init
                , shopCarnet = Tuple.first ShopCarnetPage.init
                , history = HistoryPage.init
                , account = accountModel
                , shared = Shared.init
                , login = loginModel
                , onboarding = Nothing
                , route = route
                , errors = []
                , notifications = []
                , userData = userData
                , authError = AuthErrorNone
                , navKey = navKey
                , verifyUser = Nothing
                }
    in
        ( routeModel
        , Cmd.batch
            [ Cmd.map OverviewMsg overviewCmd
            , Cmd.map AccountMsg accountCmd
            , Cmd.map LoginMsg loginCmd
            , routeCmd
            , MiscService.bodyClass "light"
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GlobalAction globalAction ->
            case globalAction of
                GA.RouteTo route ->
                    ( model, Route.newUrl model.navKey route )

                GA.SetTitle title ->
                    let
                        appInfo =
                            model.appInfo

                        newAppInfo =
                            { appInfo | title = Util.PageTitle.pageTitle title }
                    in
                        ( { model | appInfo = newAppInfo }, Cmd.none )

                GA.ShowNotification notification ->
                    ( { model | notifications = notification :: model.notifications }, Cmd.none )

                GA.SetCustomerNumber number ->
                    let
                        oldEnvironment =
                            model.environment

                        newEnvironment =
                            { oldEnvironment | customerNumber = number }
                    in
                        ( { model | environment = newEnvironment }, Cmd.none )

                GA.OpenEditTravelCard ->
                    ( { model | account = AccountPage.setEditSection (Just AccountPage.TravelCardSection) model.account }
                    , Route.newUrl model.navKey Route.Settings
                    )

                GA.FocusItem id ->
                    ( model, focusBox id )

                GA.Logout ->
                    let
                        oldEnvironment =
                            model.environment

                        newEnvironment =
                            { oldEnvironment
                                | customerId = Nothing
                                , token = ""
                            }
                    in
                        ( { model | userData = NotLoaded, environment = newEnvironment, onboarding = Nothing, authError = AuthErrorNone }
                        , Cmd.batch [ FirebaseAuth.signOut, TaskUtil.doTask <| RouteTo Route.Home ]
                        )

        SetRoute route ->
            setRoute route model

        RouteTo route ->
            ( model
            , Route.newUrl model.navKey route
            )

        UrlChanged url ->
            setRoute (Route.fromUrl url) model

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey <| Url.toString url )

                Browser.External href ->
                    ( model, Nav.load href )

        MaybeCloseNotification _ ->
            let
                newNotifications =
                    model.notifications
                        |> List.map Notification.decrementTimer
                        |> List.filter
                            (\n ->
                                case n.timer of
                                    Just time ->
                                        time > 0

                                    _ ->
                                        True
                            )
            in
                ( { model | notifications = newNotifications }, Cmd.none )

        OverviewMsg subMsg ->
            OverviewPage.update subMsg model.environment model.overview
                |> PageUpdater.map (\newModel -> { model | overview = newModel }) OverviewMsg
                |> doPageUpdate

        ShopMsg subMsg ->
            ShopPage.update subMsg model.environment model.shop model.shared
                |> PageUpdater.map (\newModel -> { model | shop = newModel }) ShopMsg
                |> doPageUpdate

        ShopCarnetMsg subMsg ->
            ShopCarnetPage.update subMsg model.environment model.shopCarnet model.shared
                |> PageUpdater.map (\newModel -> { model | shopCarnet = newModel }) ShopCarnetMsg
                |> doPageUpdate

        HistoryMsg subMsg ->
            HistoryPage.update subMsg model.environment model.history
                |> PageUpdater.map (\newModel -> { model | history = newModel }) HistoryMsg
                |> doPageUpdate

        AccountMsg subMsg ->
            AccountPage.update subMsg model.environment model.account
                |> PageUpdater.map (\newModel -> { model | account = newModel }) AccountMsg
                |> doPageUpdate

        LoginMsg subMsg ->
            LoginPage.update subMsg model.environment model.login model.navKey
                |> PageUpdater.map (\newModel -> { model | login = newModel, authError = AuthErrorNone }) LoginMsg
                |> doPageUpdate

        OnboardingMsg subMsg ->
            case model.onboarding of
                Just onboarding ->
                    OnboardingPage.update subMsg model.environment model.shared onboarding
                        |> PageUpdater.map (\newModel -> { model | onboarding = Just newModel }) OnboardingMsg
                        |> doPageUpdate

                Nothing ->
                    ( model, Cmd.none )

        VerifyUserMsg subMsg ->
            case model.verifyUser of
                Just email ->
                    VerifyUserPage.update subMsg email
                        |> PageUpdater.map (\newModel -> { model | verifyUser = Just newModel }) VerifyUserMsg
                        |> doPageUpdate

                Nothing ->
                    ( model, Cmd.none )

        SharedMsg subMsg ->
            ( { model | shared = Shared.update subMsg model.shared }, Cmd.none )

        StartOnboarding ( token, email, phone ) ->
            ( { model | onboarding = Just <| OnboardingPage.init token email phone }, Cmd.none )

        StartVerifyUser email ->
            ( { model | verifyUser = Just <| VerifyUserPage.init email }, Cmd.none )

        LogOut ->
            let
                oldEnvironment =
                    model.environment

                newEnvironment =
                    { oldEnvironment
                        | customerId = Nothing
                        , token = ""
                    }
            in
                ( { model
                    | userData = NotLoaded
                    , environment = newEnvironment
                    , onboarding = Nothing
                    , verifyUser = Nothing
                    , authError = AuthErrorNone
                  }
                , Cmd.batch [ FirebaseAuth.signOut, TaskUtil.doTask <| RouteTo Route.Home ]
                )

        CloseValidityWarning ->
            let
                oldEnvironment =
                    model.environment

                newEnvironment =
                    { oldEnvironment
                        | showValidityWarning = False
                    }
            in
                ( { model | environment = newEnvironment }
                , MiscService.closeValidityWarning ()
                )

        LoggedInData result ->
            case result of
                Ok value ->
                    let
                        oldEnvironment =
                            model.environment

                        newEnvironment =
                            { oldEnvironment
                                | customerId = Just value.customerId
                                , customerEmail = value.email
                                , token = value.token
                            }

                        newModel =
                            { model
                                | userData = Loaded value
                                , environment = newEnvironment
                                , verifyUser = Nothing
                            }
                    in
                        ( if value.stopOnboarding then
                            { newModel | onboarding = Nothing }

                          else
                            newModel
                        , -- If we are on one of the pages of the main app (so excluding login, onboarding, etc.),
                          -- enter that page.
                          case model.route of
                            Just Route.Home ->
                                TaskUtil.doTask <| OverviewMsg OverviewPage.OnEnterPage

                            Just Route.Shop ->
                                TaskUtil.doTask <| ShopMsg ShopPage.OnEnterPage

                            Just Route.History ->
                                TaskUtil.doTask <| HistoryMsg HistoryPage.OnEnterPage

                            Just Route.Settings ->
                                TaskUtil.doTask <| AccountMsg AccountPage.OnEnterPage

                            _ ->
                                Cmd.none
                        )

                Err error ->
                    ( { model | authError = AuthErrorSimple <| Decode.errorToString error }, Cmd.none )

        LoggedInError result ->
            case result of
                Ok authError ->
                    ( { model | authError = authError, userData = NotLoaded }
                    , MiscService.reloadPage ()
                    )

                Err error ->
                    ( { model | authError = AuthErrorSimple <| Decode.errorToString error }, Cmd.none )

        Focus _ ->
            -- Ignore if failing to focus item.
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    Browser.Document model.appInfo.title
        [ wrapPage
            model
            [ case ( model.userData, model.verifyUser, model.onboarding ) of
                ( Loading _, Nothing, Nothing ) ->
                    H.ul [ A.class "waiting-room" ]
                        [ H.li [] []
                        , H.li [] []
                        , H.li [] []
                        , H.li [] []
                        ]

                ( NotLoaded, Just email, Nothing ) ->
                    VerifyUserPage.view email
                        |> H.map VerifyUserMsg

                ( _, _, Just onboarding ) ->
                    OnboardingPage.view model.environment model.shared onboarding
                        |> H.map OnboardingMsg

                ( _, _, _ ) ->
                    case model.environment.customerId of
                        Just _ ->
                            viewPage model

                        Nothing ->
                            LoginPage.view model.environment model.login
                                |> H.map LoginMsg
            ]
        ]


wrapPage : Model -> List (Html Msg) -> Html Msg
wrapPage model children =
    let
        showVisibility =
            case ( model.userData, model.verifyUser, model.onboarding ) of
                ( _, Just _, Just _ ) ->
                    False

                _ ->
                    model.environment.showValidityWarning && model.environment.customerId /= Nothing
    in
        H.div [ A.class "light container" ]
            [ viewAuthError model
            , header model
            , Html.Extra.viewIf showVisibility (viewValidityWarning model)
            , H.main_ [ A.class "app" ]
                [ Ui.GlobalNotifications.notifications model.notifications
                , H.div [ A.class "content" ]
                    children
                ]
            ]


viewValidityWarning : Model -> Html Msg
viewValidityWarning model =
    if model.route == Just Route.Home then
        H.div [ A.class "validityWarning" ]
            [ H.div [ A.class "validityWarning__inner", A.attribute "role" "alert" ]
                [ Icon.info
                , H.text "Obs! Husk at nettbutikken ikke kan brukes som billettbevis i en eventuell kontroll."
                , H.button
                    [ E.onClick CloseValidityWarning
                    , A.type_ "button"
                    , A.class "validityWarning__button"
                    , A.title "Fjern melding"
                    ]
                    [ Icon.cross, Ui.ScreenReaderText.view "Fjern melding" ]
                ]
            ]

    else
        Html.Extra.nothing


header : Model -> Html Msg
header model =
    let
        links =
            [ ( "Ny periodebillett", Route.Shop, Shared.hasPeriodTickets model.shared )
            , ( "Nytt klippekort", Route.ShopCarnet, Shared.hasCarnetTickets model.shared )
            , ( "Kjøpshistorikk", Route.History, True )
            , ( "Min profil", Route.Settings, True )
            ]

        showHeader =
            model.environment.customerId /= Nothing || model.onboarding /= Nothing || model.verifyUser /= Nothing

        navigation =
            if model.onboarding == Nothing && model.verifyUser == Nothing then
                List.map
                    (\( name, route, visible ) ->
                        if not visible then
                            Html.Extra.nothing

                        else
                            H.li
                                [ A.classList
                                    [ ( "pageHeader__nav__item", True )
                                    , ( "pageHeader__nav__item--active", Just route == model.route )
                                    ]
                                ]
                                [ H.a [ Route.href route ] [ H.text name ] ]
                    )
                    links

            else
                []
    in
        H.header [ A.class "pageHeader" ]
            [ H.div [ A.class "pageHeader__content" ]
                [ H.h1 [ A.class "pageHeader__logo" ]
                    [ H.a [ Route.href Route.Home ] [ Icon.atb, H.text "AtB Nettbutikk" ]
                    ]
                , if showHeader then
                    H.nav [ A.class "pageHeader__nav" ]
                        [ H.ul []
                            (navigation
                                ++ [ H.li []
                                        [ H.button [ A.class "pageHeader__nav__logout", E.onClick LogOut ] [ H.text "Logg ut", Icon.logout ]
                                        ]
                                   ]
                            )
                        ]

                  else
                    Html.Extra.nothing
                ]
            ]


{-| Always show error box, but offset to top and position absolute to animate in/out
-}
viewAuthError : Model -> Html Msg
viewAuthError model =
    let
        hasError =
            model.authError /= AuthErrorNone

        classList =
            [ ( "authErrorBox", True )
            , ( "authErrorBox--active", hasError )
            ]

        classListChild =
            [ ( "authErrorBox__content", True )
            , ( "authErrorBox__content--active", hasError )
            ]
    in
        H.div [ A.classList classList ]
            [ H.div [ A.classList classListChild ]
                [ case model.authError of
                    AuthErrorSimple message ->
                        Ui.Message.error message

                    _ ->
                        Ui.Message.error ""
                ]
            ]


viewPage : Model -> Html Msg
viewPage model =
    let
        env =
            model.environment

        shared =
            model.shared
    in
        case model.route of
            Just Route.Shop ->
                ShopPage.view env model.appInfo shared model.shop model.route
                    |> H.map ShopMsg

            Just Route.ShopCarnet ->
                ShopCarnetPage.view env model.appInfo shared model.shopCarnet model.route
                    |> H.map ShopCarnetMsg

            Just Route.History ->
                HistoryPage.view env model.appInfo shared model.history model.route
                    |> H.map HistoryMsg
                    |> wrapSubPage "Kjøpshistorikk"

            Just Route.Settings ->
                AccountPage.view env model.appInfo shared model.account model.route
                    |> H.map AccountMsg
                    |> wrapSubPage "Min profil"

            _ ->
                OverviewPage.view env model.appInfo shared model.overview model.route
                    |> H.map OverviewMsg


wrapSubPage : String -> Html msg -> Html msg
wrapSubPage title children =
    H.div []
        [ PH.init |> PH.setTitle (Just title) |> PH.setBackRoute ( "Oversikt", Route.Home ) |> PH.view
        , children
        ]


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ OverviewPage.subscriptions model.overview
            |> Sub.map OverviewMsg
        , ShopPage.subscriptions model.shop
            |> Sub.map ShopMsg
        , HistoryPage.subscriptions model.history
            |> Sub.map HistoryMsg
        , AccountPage.subscriptions model.account
            |> Sub.map AccountMsg
        , LoginPage.subscriptions model.login
            |> Sub.map LoginMsg
        , Shared.subscriptions
            |> Sub.map SharedMsg
        , VerifyUserPage.subscriptions
            |> Sub.map VerifyUserMsg
        , Time.every 1000 MaybeCloseNotification
        , FirebaseAuth.signedInInfo (Decode.decodeValue userDataDecoder >> LoggedInData)
        , FirebaseAuth.signInError (Decode.decodeValue userErrorDecoder >> LoggedInError)
        , MiscService.onboardingStart StartOnboarding
        , FirebaseAuth.verifyUserStart StartVerifyUser
        ]



--


type AuthError
    = AuthErrorNone
    | AuthErrorSimple String


type alias UserData =
    { token : String
    , email : String
    , userId : String
    , customerId : String
    , provider : FirebaseAuth.Provider
    , stopOnboarding : Bool
    }


userDataDecoder : Decoder UserData
userDataDecoder =
    Decode.succeed UserData
        |> DecodeP.required "token" Decode.string
        |> DecodeP.required "email" Decode.string
        |> DecodeP.required "uid"
            (Decode.map
                (\uid -> "ATB:CustomerAccount:" ++ uid)
                Decode.string
            )
        |> DecodeP.required "uid" Decode.string
        |> DecodeP.required "provider" FirebaseAuth.providerDecoder
        |> DecodeP.optional "stopOnboarding" Decode.bool False


userErrorDecoder : Decoder AuthError
userErrorDecoder =
    Decode.map AuthErrorSimple (Decode.field "message" Decode.string)



--


doPageUpdate : PageUpdater Model Msg -> ( Model, Cmd Msg )
doPageUpdate pageUpdater =
    let
        ( newModel, cmd ) =
            pageUpdater.update
    in
        ( newModel, Cmd.batch (cmd :: List.map (GlobalAction >> TaskUtil.doTask) pageUpdater.globalActions) )


focusBox : Maybe String -> Cmd Msg
focusBox id =
    case id of
        Just a ->
            Task.attempt Focus (Dom.focus a)

        Nothing ->
            Cmd.none
