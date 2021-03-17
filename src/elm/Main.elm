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
import Html.Events as E
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodeP
import Notification exposing (Notification)
import Page.History as HistoryPage
import Page.Home as HomePage
import Page.Onboarding as OnboardingPage
import Page.Settings as SettingsPage
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
    | HomeMsg HomePage.Msg
    | ShopMsg ShopPage.Msg
    | HistoryMsg HistoryPage.Msg
    | SettingsMsg SettingsPage.Msg
    | OnboardingMsg OnboardingPage.Msg
    | SharedMsg Shared.Msg
    | StartOnboarding String
    | LogIn FirebaseAuth.Provider
    | LogOut
    | LoggedInData (Result Decode.Error UserData)
    | LoggedInError (Result Decode.Error AuthError)


type alias Model =
    { environment : Environment
    , appInfo : AppInfo
    , home : HomePage.Model
    , shop : Maybe ShopPage.Model
    , history : HistoryPage.Model
    , settings : SettingsPage.Model
    , shared : Shared
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

        ( homeModel, homeCmd ) =
            HomePage.init

        ( settingsModel, settingsCmd ) =
            SettingsPage.init

        historyModel =
            HistoryPage.init

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
                , home = homeModel
                , shop = Nothing
                , history = historyModel
                , settings = settingsModel
                , shared = Shared.init
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
            [ Cmd.map HomeMsg homeCmd
            , Cmd.map SettingsMsg settingsCmd
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

                GA.RefreshTickets ->
                    ( model, Cmd.none )

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

        HomeMsg subMsg ->
            HomePage.update subMsg model.environment model.home
                |> PageUpdater.map (\newModel -> { model | home = newModel }) HomeMsg
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

        SettingsMsg subMsg ->
            SettingsPage.update subMsg model.environment model.settings
                |> PageUpdater.map (\newModel -> { model | settings = newModel }) SettingsMsg
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

        StartOnboarding token ->
            ( { model | onboarding = Just <| OnboardingPage.init token }, Cmd.none )

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
                H.div []
                    [ header model
                    , case model.environment.customerId of
                        Just _ ->
                            H.main_ [] [ viewPage model ]

                        Nothing ->
                            case model.onboarding of
                                Just onboarding ->
                                    OnboardingPage.view model.environment onboarding
                                        |> H.map OnboardingMsg

                                Nothing ->
                                    viewLogin model
                    ]
        ]


viewLogin : Model -> Html msg
viewLogin _ =
    H.div [] [ H.text "login" ]


header : Model -> Html Msg
header model =
    H.header []
        [ Icon.atb
        , H.span [] []
        , case model.userData of
            Loaded userData ->
                H.div []
                    [ H.text userData.email
                    , H.text " | "
                    , H.text userData.userId
                    , H.text " | "
                    , H.button [ E.onClick LogOut ] [ H.text "Log out" ]
                    ]

            _ ->
                H.div []
                    [ H.button [ E.onClick (LogIn FirebaseAuth.Anonymous) ] [ H.text "Log in anonymously" ]
                    , H.button [ E.onClick (LogIn FirebaseAuth.Google) ] [ H.text "Log in with Google" ]
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
            Just Route.Home ->
                HomePage.view env model.appInfo shared model.home model.route
                    |> H.map HomeMsg

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
                SettingsPage.view env model.appInfo shared model.settings model.route
                    |> H.map SettingsMsg
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
        [ HomePage.subscriptions model.home
            |> Sub.map HomeMsg
        , model.shop
            |> Maybe.map (ShopPage.subscriptions >> Sub.map ShopMsg)
            |> Maybe.withDefault Sub.none
        , HistoryPage.subscriptions model.history
            |> Sub.map HistoryMsg
        , SettingsPage.subscriptions model.settings
            |> Sub.map SettingsMsg
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
