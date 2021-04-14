module Main exposing (main)

import Base exposing (AppInfo)
import Browser
import Browser.Navigation as Nav
import Environment exposing (DistributionEnvironment(..), Environment, Language(..))
import Error exposing (Error)
import Fragment.Icon as Icon
import GlobalActions as GA exposing (GlobalAction)
import Html as H exposing (Html)
import Html.Attributes as A
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodeP
import Notification exposing (Notification)
import Page.Account as AccountPage
import Page.History as HistoryPage
import Page.Login as LoginPage
import Page.Onboarding as OnboardingPage
import Page.Overview as OverviewPage
import Page.Shop as ShopPage
import PageUpdater exposing (PageUpdater)
import Route exposing (Route)
import Service.FirebaseAuth as FirebaseAuth
import Service.Misc as MiscService
import Shared exposing (Shared)
import Time
import Url exposing (Url)
import Util.Status exposing (Status(..))
import Util.Task as TaskUtil


type Msg
    = GlobalAction (GlobalAction Msg)
    | SetRoute (Maybe Route)
    | RouteTo Route
    | UrlChanged Url
    | UrlRequested Browser.UrlRequest
    | CloseNotification Time.Posix
    | OverviewMsg OverviewPage.Msg
    | ShopMsg ShopPage.Msg
    | HistoryMsg HistoryPage.Msg
    | AccountMsg AccountPage.Msg
    | LoginMsg LoginPage.Msg
    | OnboardingMsg OnboardingPage.Msg
    | SharedMsg Shared.Msg
    | StartOnboarding ( String, String, String )
    | LogIn FirebaseAuth.Provider
    | LogOut
    | LoggedInData (Result Decode.Error UserData)
    | LoggedInError (Result Decode.Error AuthError)


type alias Model =
    { environment : Environment
    , appInfo : AppInfo
    , overview : OverviewPage.Model
    , shop : Maybe ShopPage.Model
    , history : HistoryPage.Model
    , account : AccountPage.Model
    , shared : Shared
    , login : LoginPage.Model
    , onboarding : Maybe OnboardingPage.Model
    , route : Maybe Route.Route
    , errors : List Error
    , notifications : List (Notification Msg)
    , userData : Status UserData
    , authError : AuthError
    , navKey : Nav.Key
    }


type alias Flags =
    { isDevelopment : Bool
    , baseUrl : String
    , ticketUrl : String
    , refDataUrl : String
    , version : String
    , commit : String
    , installId : String
    , loggedIn : Bool
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
setRoute maybeRoute model =
    ( { model | route = maybeRoute }
    , case maybeRoute of
        Just Route.Shop ->
            if model.shop == Nothing then
                TaskUtil.doTask <| RouteTo Route.Home

            else
                Cmd.none

        Just _ ->
            Cmd.none

        Nothing ->
            TaskUtil.doTask <| RouteTo Route.Home
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
            , baseUrl = flags.baseUrl
            , ticketUrl = flags.ticketUrl
            , refDataUrl = flags.refDataUrl
            , language = English
            , installId = flags.installId
            , customerId = Nothing
            , customerNumber = 0
            , customerEmail = ""
            , token = ""
            }

        appInfo =
            { title = "AtB Webshop"
            , version = flags.version
            , commit = flags.commit
            }

        ( overviewModel, overviewCmd ) =
            OverviewPage.init

        ( accountModel, accountCmd ) =
            AccountPage.init

        route =
            Route.fromUrl url

        userData =
            if flags.loggedIn then
                Loading Nothing

            else
                NotLoaded

        ( routeModel, routeCmd ) =
            setRoute route
                { environment = environment
                , appInfo = appInfo
                , overview = overviewModel
                , shop = Nothing
                , history = HistoryPage.init
                , account = accountModel
                , shared = Shared.init
                , login = LoginPage.init
                , onboarding = Nothing
                , route = route
                , errors = []
                , notifications = []
                , userData = userData
                , authError = AuthErrorNone
                , navKey = navKey
                }
    in
        ( routeModel
        , Cmd.batch
            [ Cmd.map OverviewMsg overviewCmd
            , Cmd.map AccountMsg accountCmd
            , routeCmd
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GlobalAction globalAction ->
            case globalAction of
                GA.RouteTo route ->
                    ( model, Route.newUrl model.navKey route )

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

                GA.OpenShop ->
                    let
                        ( initModel, initCmd ) =
                            ShopPage.init model.shared
                    in
                        ( { model | shop = Just initModel }
                        , Cmd.batch
                            [ Cmd.map ShopMsg initCmd
                            , Route.newUrl model.navKey Route.Shop
                            ]
                        )

                GA.CloseShop ->
                    ( { model | shop = Nothing }, Route.newUrl model.navKey Route.Home )

                GA.SetPendingOrder orderId ->
                    ( model
                    , TaskUtil.doTask <| OverviewMsg <| OverviewPage.SetPendingOrder orderId
                    )

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
                        ( { model | userData = NotLoaded, environment = newEnvironment, authError = AuthErrorNone }
                        , FirebaseAuth.signOut
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

        CloseNotification _ ->
            let
                newNotifications =
                    model.notifications
                        |> List.tail
                        |> Maybe.withDefault []
            in
                ( { model | notifications = newNotifications }, Cmd.none )

        OverviewMsg subMsg ->
            OverviewPage.update subMsg model.environment model.overview
                |> PageUpdater.map (\newModel -> { model | overview = newModel }) OverviewMsg
                |> doPageUpdate

        ShopMsg subMsg ->
            case model.shop of
                Just shop ->
                    ShopPage.update subMsg model.environment shop
                        |> PageUpdater.map (\newModel -> { model | shop = Just newModel }) ShopMsg
                        |> doPageUpdate

                Nothing ->
                    ( model, Cmd.none )

        HistoryMsg subMsg ->
            HistoryPage.update subMsg model.environment model.history
                |> PageUpdater.map (\newModel -> { model | history = newModel }) HistoryMsg
                |> doPageUpdate

        AccountMsg subMsg ->
            AccountPage.update subMsg model.environment model.account
                |> PageUpdater.map (\newModel -> { model | account = newModel }) AccountMsg
                |> doPageUpdate

        LoginMsg subMsg ->
            LoginPage.update subMsg model.environment model.login
                |> PageUpdater.map (\newModel -> { model | login = newModel }) LoginMsg
                |> doPageUpdate

        OnboardingMsg subMsg ->
            case model.onboarding of
                Just onboarding ->
                    OnboardingPage.update subMsg model.environment onboarding
                        |> PageUpdater.map (\newModel -> { model | onboarding = Just newModel }) OnboardingMsg
                        |> doPageUpdate

                Nothing ->
                    ( model, Cmd.none )

        SharedMsg subMsg ->
            ( { model | shared = Shared.update subMsg model.shared }, Cmd.none )

        StartOnboarding ( token, email, phone ) ->
            ( { model | onboarding = Just <| OnboardingPage.init token email phone }, Cmd.none )

        LogIn provider ->
            ( model, FirebaseAuth.signIn provider )

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
                ( { model | userData = NotLoaded, environment = newEnvironment, authError = AuthErrorNone }
                , FirebaseAuth.signOut
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
                    in
                        ( { model | userData = Loaded value, environment = newEnvironment }
                        , Cmd.none
                        )

                Err error ->
                    ( { model | authError = AuthErrorSimple <| Decode.errorToString error }, Cmd.none )

        LoggedInError result ->
            case result of
                Ok value ->
                    ( { model | authError = value }, Cmd.none )

                Err error ->
                    ( { model | authError = AuthErrorSimple <| Decode.errorToString error }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    Browser.Document model.appInfo.title
        [ case model.userData of
            Loading _ ->
                case model.onboarding of
                    Just onboarding ->
                        OnboardingPage.view model.environment onboarding
                            |> H.map OnboardingMsg

                    Nothing ->
                        H.ul [ A.class "waiting-room" ]
                            [ H.li [] []
                            , H.li [] []
                            , H.li [] []
                            , H.li [] []
                            ]

            _ ->
                H.div [ A.class "light container" ]
                    [ header model
                    , case model.environment.customerId of
                        Just _ ->
                            H.main_ [ A.class "app" ] [ viewPage model ]

                        Nothing ->
                            case model.onboarding of
                                Just onboarding ->
                                    OnboardingPage.view model.environment onboarding
                                        |> H.map OnboardingMsg

                                Nothing ->
                                    LoginPage.view model.environment model.login
                                        |> H.map LoginMsg
                    ]
        ]


header : Model -> Html Msg
header _ =
    H.header [ A.class "pageHeader" ] [ Icon.atb ]


viewPage : Model -> Html Msg
viewPage model =
    let
        env =
            model.environment

        shared =
            model.shared
    in
        case model.route of
            Just Route.Home ->
                OverviewPage.view env model.appInfo shared model.overview model.route
                    |> H.map OverviewMsg

            Just Route.Shop ->
                case model.shop of
                    Just shop ->
                        ShopPage.view env model.appInfo shared shop model.route
                            |> H.map ShopMsg
                            |> wrapSubPage "Kjøp ny billett"

                    Nothing ->
                        H.text ""

            Just Route.History ->
                HistoryPage.view env model.appInfo shared model.history model.route
                    |> H.map HistoryMsg
                    |> wrapSubPage "Kjøpshistorikk"

            Just Route.Settings ->
                AccountPage.view env model.appInfo shared model.account model.route
                    |> H.map AccountMsg
                    |> wrapSubPage "Kontoinformasjon"

            Just Route.NotFound ->
                H.div
                    [ A.class "welcome-container" ]
                    [ H.div []
                        [ H.h2 [] [ H.text model.appInfo.title ]
                        , H.h3 [] [ H.text "Not found." ]
                        ]
                    ]

            Nothing ->
                H.div
                    [ A.class "welcome-container" ]
                    [ H.div []
                        [ H.h2 [] [ H.text model.appInfo.title ]
                        , H.h3 [] [ H.a [ A.href "#/" ] [ H.text "Go home" ] ]
                        , H.h3 [] [ H.a [ A.href "#/settings" ] [ H.text "Go to settings" ] ]
                        ]
                    ]


wrapSubPage : String -> Html msg -> Html msg
wrapSubPage title children =
    H.div []
        [ H.div [ A.class "title-bar" ]
            [ H.div [ A.class "go-back" ] [ H.a [ A.href "#/" ] [ Icon.leftArrow, H.text " Oversikt" ] ]
            , H.div [] [ H.text title ]
            ]
        , children
        ]


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ OverviewPage.subscriptions model.overview
            |> Sub.map OverviewMsg
        , model.shop
            |> Maybe.map (ShopPage.subscriptions >> Sub.map ShopMsg)
            |> Maybe.withDefault Sub.none
        , HistoryPage.subscriptions model.history
            |> Sub.map HistoryMsg
        , AccountPage.subscriptions model.account
            |> Sub.map AccountMsg
        , LoginPage.subscriptions model.login
            |> Sub.map LoginMsg
        , Shared.subscriptions
            |> Sub.map SharedMsg
        , model.notifications
            |> List.head
            |> Maybe.map (\_ -> Time.every 1000 CloseNotification)
            |> Maybe.withDefault Sub.none
        , FirebaseAuth.signInInfo (Decode.decodeValue userDataDecoder >> LoggedInData)
        , FirebaseAuth.signInError (Decode.decodeValue userDataDecoder >> LoggedInData)
        , MiscService.onboardingStart StartOnboarding
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



--


doPageUpdate : PageUpdater Model Msg -> ( Model, Cmd Msg )
doPageUpdate pageUpdater =
    let
        ( newModel, cmd ) =
            pageUpdater.update
    in
        ( newModel, Cmd.batch (cmd :: List.map (GlobalAction >> TaskUtil.doTask) pageUpdater.globalActions) )
