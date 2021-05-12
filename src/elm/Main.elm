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
import Task
import Time
import Ui.GlobalNotifications
import Ui.Message
import Ui.PageHeader as PH
import Url exposing (Url)
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
    | Focus (Result Dom.Error ())


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
    , localUrl : String
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
                TaskUtil.doTask <| GlobalAction GA.OpenShop

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
            , localUrl = flags.localUrl
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
            setRoute route
                { environment = environment
                , appInfo = appInfo
                , overview = overviewModel
                , shop = Nothing
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
                }
    in
        ( routeModel
        , Cmd.batch
            [ Cmd.map OverviewMsg overviewCmd
            , Cmd.map AccountMsg accountCmd
            , Cmd.map LoginMsg loginCmd
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

                GA.OpenEditTravelCard ->
                    ( { model | account = AccountPage.setEditSection (Just AccountPage.TravelCardSection) model.account }
                    , Route.newUrl model.navKey Route.Settings
                    )

                GA.CloseShop ->
                    ( { model | shop = Nothing }, Route.newUrl model.navKey Route.Home )

                GA.SetPendingOrder orderId ->
                    ( model
                    , TaskUtil.doTask <| OverviewMsg <| OverviewPage.SetPendingOrder orderId
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
                        ( { model | userData = NotLoaded, environment = newEnvironment, authError = AuthErrorNone }
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
            case model.shop of
                Just shop ->
                    ShopPage.update subMsg model.environment shop model.shared
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
                |> PageUpdater.map (\newModel -> { model | login = newModel, authError = AuthErrorNone }) LoginMsg
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
                , Cmd.batch [ FirebaseAuth.signOut, TaskUtil.doTask <| RouteTo Route.Home ]
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
                            { model | userData = Loaded value, environment = newEnvironment }
                    in
                        ( if value.stopOnboarding then
                            { newModel | onboarding = Nothing }

                          else
                            newModel
                        , Cmd.none
                        )

                Err error ->
                    ( { model | authError = AuthErrorSimple <| Decode.errorToString error }, Cmd.none )

        LoggedInError result ->
            case result of
                Ok authError ->
                    ( { model | authError = authError }, Cmd.none )

                Err error ->
                    ( { model | authError = AuthErrorSimple <| Decode.errorToString error }, Cmd.none )

        Focus _ ->
            -- Ignore if failing to focus item.
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    Browser.Document model.appInfo.title
        [ case model.userData of
            Loading _ ->
                H.ul [ A.class "waiting-room" ]
                    [ H.li [] []
                    , H.li [] []
                    , H.li [] []
                    , H.li [] []
                    ]

            _ ->
                H.div [ A.class "light container" ]
                    [ viewAuthError model
                    , header model
                    , H.main_ [ A.class "app" ]
                        [ Ui.GlobalNotifications.notifications model.notifications
                        , H.div [ A.class "content" ]
                            [ case model.onboarding of
                                Just onboarding ->
                                    OnboardingPage.view model.environment onboarding
                                        |> H.map OnboardingMsg

                                Nothing ->
                                    case model.environment.customerId of
                                        Just _ ->
                                            viewPage model

                                        Nothing ->
                                            LoginPage.view model.environment model.login
                                                |> H.map LoginMsg
                            ]
                        ]
                    ]
        ]


header : Model -> Html Msg
header model =
    let
        links =
            [ ( "Kjøp billett", Route.Shop )
            , ( "Kjøpshistorikk", Route.History )
            , ( "Min profil", Route.Settings )
            ]

        isLoggedIn =
            model.environment.customerId /= Nothing

        navigation =
            if model.onboarding == Nothing then
                List.map
                    (\( name, route ) ->
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

        showHeader =
            model.environment.customerId /= Nothing && model.route /= Just Route.Thanks
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

            Just Route.Thanks ->
                viewSuccessTicketPage

            _ ->
                OverviewPage.view env model.appInfo shared model.overview model.route
                    |> H.map OverviewMsg


wrapSubPage : String -> Html msg -> Html msg
wrapSubPage title children =
    H.div []
        [ PH.init |> PH.setTitle (Just title) |> PH.setBackRoute ( Route.Home, "Oversikt" ) |> PH.view
        , children
        ]


viewSuccessTicketPage : Html msg
viewSuccessTicketPage =
    H.div [ A.class "pageHome__successBuy" ]
        [ H.img [ A.src "/images/empty-illustration.svg" ] []
        , H.p [] [ H.text "Takk! Du kan nå lukke vinduet." ]
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
        , Time.every 1000 MaybeCloseNotification
        , FirebaseAuth.signInInfo (Decode.decodeValue userDataDecoder >> LoggedInData)
        , FirebaseAuth.signInError (Decode.decodeValue userErrorDecoder >> LoggedInError)
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
