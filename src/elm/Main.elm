module Main exposing (main)

import Base exposing (AppInfo)
import Browser
import Browser.Navigation as Nav
import Environment exposing (DistributionEnvironment(..), Environment, Language(..))
import Error exposing (Error)
import GlobalActions as GA exposing (GlobalAction)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodeP
import Notification exposing (Notification)
import Page.Home as HomePage
import Page.Settings as SettingsPage
import Route exposing (Route)
import Service.FirebaseAuth as FirebaseAuth
import Svg as S
import Svg.Attributes as SA
import Time
import Url exposing (Url)
import Util.Task as TaskUtil


type Msg
    = GlobalAction (GlobalAction Msg)
    | SetRoute (Maybe Route)
    | RouteTo Route
    | UrlChanged Url
    | UrlRequested Browser.UrlRequest
    | CloseNotification Time.Posix
    | HomeMsg HomePage.Msg
    | SettingsMsg SettingsPage.Msg
    | LogIn
    | LogOut
    | LoggedInData (Result Decode.Error UserData)
    | LoggedInError (Result Decode.Error AuthError)


type alias Model =
    { environment : Environment
    , appInfo : AppInfo
    , home : HomePage.Model
    , settings : SettingsPage.Model
    , route : Maybe Route.Route
    , errors : List Error
    , notifications : List (Notification Msg)
    , userData : Maybe UserData
    , authError : AuthError
    , navKey : Nav.Key
    }


type alias Flags =
    { isDevelopment : Bool
    , baseUrl : String
    , version : String
    , commit : String
    , installId : String
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

        environment =
            { distributionEnv = distributionEnv
            , baseUrl = flags.baseUrl
            , language = English
            , installId = flags.installId
            , customerId = Nothing
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

        route =
            Route.fromUrl url

        ( routeModel, routeCmd ) =
            setRoute route
                { environment = environment
                , appInfo = appInfo
                , home = homeModel
                , settings = settingsModel
                , route = route
                , errors = []
                , notifications = []
                , userData = Nothing
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
            let
                ( newModel, newCmd ) =
                    HomePage.update subMsg model.environment model.home
            in
                ( { model | home = newModel }
                , Cmd.map HomeMsg newCmd
                )

        SettingsMsg subMsg ->
            let
                ( newModel, newCmd ) =
                    SettingsPage.update subMsg model.environment model.settings
            in
                ( { model | settings = newModel }
                , Cmd.map SettingsMsg newCmd
                )

        LogIn ->
            ( model, FirebaseAuth.signIn () )

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
                ( { model | userData = Nothing, environment = newEnvironment, authError = AuthErrorNone }
                , FirebaseAuth.signOut ()
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
                                , token = value.token
                            }
                    in
                        ( { model | userData = Just value, environment = newEnvironment }
                        , TaskUtil.doTask (HomeMsg HomePage.LoadAccount)
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
    Browser.Document model.appInfo.title [ viewInternal model ]


viewInternal : Model -> Html Msg
viewInternal model =
    H.div []
        [ header model
        , viewPage model
        ]


header : Model -> Html Msg
header model =
    H.header []
        [ S.svg [ SA.id "logo_atb_graa", SA.viewBox "0 0 383.764 383.657" ]
            [ S.defs [] [ S.style [] [ S.text ".atb-logo-cls-1{fill:#fff;}" ] ]
            , S.path [ SA.id "logo_atb_graa_ramme", SA.class "atb-logo-cls-1", SA.d "M357.187,0h-.009L105.66.11A105.787,105.787,0,0,0,0,105.779l.105,251.31A26.6,26.6,0,0,0,26.68,383.657H357.187a26.607,26.607,0,0,0,26.577-26.577V26.577A26.607,26.607,0,0,0,357.187,0ZM105.669,21.037l251.518-.11a5.619,5.619,0,0,1,3.334,1.095c-7.73,20.742-24.879,35.236-49.262,46.711l-1.871.97C267.95,86.443,218.4,94.728,184.125,98.732c-7.754.906-15.41,1.72-22.821,2.508-35.986,3.828-69.979,7.445-99.445,19.355a140.171,140.171,0,0,0-26.77,14.175l-.809.533q-6.677,4.248-13.337,9.336l-.016-38.86A84.742,84.742,0,0,1,105.669,21.037ZM357.187,362.731H26.68a5.649,5.649,0,0,1-5.648-5.651l-.069-166.018c8.4-8.381,19.625-18.368,32.184-26.342l1.294-.85.228-.154A105.821,105.821,0,0,1,74.951,153c24.933-10.078,56.564-13.443,90.056-17.007,7.493-.8,15.241-1.62,23.174-2.547,36.407-4.252,89.255-13.14,134.3-31.338a178.442,178.442,0,0,0,40.357-22.514V357.08A5.651,5.651,0,0,1,357.187,362.731Z" ] []
            , S.path [ SA.id "logo_atb_graa_tekst", SA.class "atb-logo-cls-1", SA.d "M196.082,331.462c-8.551,0-13.115-1.731-15.562-3.586a15.775,15.775,0,0,1-5.362-8.24,50.139,50.139,0,0,1-1.686-14.394V248.061h-30.1l20.366,83.389H124.431l-3.773-24.819H91.272L87.5,331.45H48.786L82.735,189.687H129.2l8.416,34.349h35.861v-31.57h35.739v31.57h14.1v24.025h-14.1v51.225q0,4.571,2.085,6.452t6.452,1.886h16.086l-.057-117.937h55.645q19.849,0,29.285,9.629t9.432,25.711a45.4,45.4,0,0,1-1.29,10.821,32.191,32.191,0,0,1-3.974,9.531,25.4,25.4,0,0,1-6.848,7.247A25.975,25.975,0,0,1,305.9,256.8v.395a26.633,26.633,0,0,1,11.913,3.673,27.425,27.425,0,0,1,8.14,7.644,32.426,32.426,0,0,1,4.666,10.027,41.727,41.727,0,0,1,1.489,11.02,57.2,57.2,0,0,1-2.382,16.878,34.528,34.528,0,0,1-7.446,13.3,33.532,33.532,0,0,1-13.2,8.638q-8.143,3.078-19.655,3.076H232.929ZM105.768,218.675l-10.126,60.16h20.649l-10.125-60.16Zm173.725,26.6a9.968,9.968,0,0,0,8.737-4.269,17.061,17.061,0,0,0,2.978-10.024,17.881,17.881,0,0,0-2.978-10.326,9.907,9.907,0,0,0-8.737-4.367h-8.934v28.986Zm1.391,59.566a12.153,12.153,0,0,0,9.132-4.269q3.972-4.266,3.971-13.2,0-8.34-3.971-12.709a12.056,12.056,0,0,0-9.132-4.367H270.559v34.548Z" ] []
            ]
        , H.span [] []
        , case model.userData of
            Just userData ->
                H.div []
                    [ H.text userData.email
                    , H.text " | "
                    , H.text userData.userId
                    , H.text " | "
                    , H.button [ E.onClick LogOut ] [ H.text "Log out" ]
                    ]

            Nothing ->
                H.div [] [ H.button [ E.onClick LogIn ] [ H.text "Log in" ] ]
        ]


viewPage : Model -> Html Msg
viewPage model =
    let
        env =
            model.environment
    in
        case model.route of
            Just Route.Home ->
                HomePage.view env model.appInfo model.home model.route
                    |> H.map HomeMsg

            Just Route.Settings ->
                SettingsPage.view env model.appInfo model.settings model.route
                    |> H.map SettingsMsg

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


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ HomePage.subscriptions model.home
            |> Sub.map HomeMsg
        , SettingsPage.subscriptions model.settings
            |> Sub.map SettingsMsg
        , model.notifications
            |> List.head
            |> Maybe.map (\_ -> Time.every 1000 CloseNotification)
            |> Maybe.withDefault Sub.none
        , FirebaseAuth.signInInfo (Decode.decodeValue userDataDecoder >> LoggedInData)
        , FirebaseAuth.signInError (Decode.decodeValue userDataDecoder >> LoggedInData)
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
    }


userDataDecoder : Decoder UserData
userDataDecoder =
    Decode.succeed UserData
        |> DecodeP.required "token" Decode.string
        |> DecodeP.required "email" Decode.string
        |> DecodeP.required "uid" Decode.string
        |> DecodeP.required "uid"
            (Decode.andThen
                (\uid ->
                    case String.split ":" uid of
                        [ "ATB", "CustomerAccount", customerId ] ->
                            Decode.succeed customerId

                        _ ->
                            Decode.fail "Invalid customer id"
                )
                Decode.string
            )
