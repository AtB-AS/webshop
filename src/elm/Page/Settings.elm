module Page.Settings exposing (Model, Msg(..), init, subscriptions, update, view)

import Base exposing (AppInfo)
import Data.Webshop exposing (Profile)
import Environment exposing (Environment)
import Fragment.Button as Button
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Http
import PageUpdater exposing (PageUpdater)
import Route exposing (Route)
import Service.Webshop as WebshopService
import Task


type Msg
    = GetProfile
    | ReceiveProfile (Result Http.Error Profile)
    | UpdateFirstName String
    | UpdateLastName String
    | UpdateProfile
    | ReceiveUpdateProfile (Result Http.Error ())


type alias Model =
    { firstName : String
    , lastName : String
    , profile : Maybe Profile
    , updating : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { firstName = ""
      , lastName = ""
      , profile = Nothing
      , updating = False
      }
    , Cmd.none
    )


update : Msg -> Environment -> Model -> PageUpdater Model Msg
update msg env model =
    case msg of
        GetProfile ->
            PageUpdater.fromPair ( model, fetchProfile env )

        ReceiveProfile result ->
            case result of
                Ok profile ->
                    PageUpdater.init
                        { model
                            | profile = Just profile
                            , firstName = profile.firstName
                            , lastName = profile.lastName
                            , updating = False
                        }
                        |> PageUpdater.addGlobalAction (GA.SetCustomerNumber profile.customerNumber)

                Err _ ->
                    PageUpdater.init model

        UpdateFirstName value ->
            PageUpdater.init { model | firstName = value }

        UpdateLastName value ->
            PageUpdater.init { model | lastName = value }

        UpdateProfile ->
            PageUpdater.fromPair
                ( { model | updating = True }
                , updateProfile env model.firstName model.lastName
                )

        ReceiveUpdateProfile result ->
            case result of
                Ok () ->
                    PageUpdater.fromPair ( model, fetchProfile env )

                Err _ ->
                    PageUpdater.init { model | updating = False }


view : Environment -> AppInfo -> Model -> Maybe Route -> Html Msg
view _ _ model _ =
    H.div [ A.class "box" ]
        [ H.h2 [] [ H.text "Profile" ]
        , H.button [ E.onClick GetProfile ] [ H.text "Refresh" ]
        , H.div [] [ viewProfile model.profile ]
        , H.h2 [] [ H.text "Update profile" ]
        , H.dl []
            [ H.dt [] [ H.text "First name: " ]
            , H.dd []
                [ H.input
                    [ A.value model.firstName
                    , E.onInput UpdateFirstName
                    , A.placeholder "First name"
                    , A.disabled model.updating
                    ]
                    []
                ]
            , H.dt [] [ H.text "Last name: " ]
            , H.dd []
                [ H.input
                    [ A.value model.lastName
                    , E.onInput UpdateLastName
                    , A.placeholder "Last name"
                    , A.disabled model.updating
                    ]
                    []
                ]
            ]
        , H.button
            [ A.disabled (String.trim model.firstName == "" || String.trim model.lastName == "" || model.updating)
            , E.onClick UpdateProfile
            ]
            [ if model.updating then
                Button.loading

              else
                H.text "Update"
            ]
        ]


viewProfile : Maybe Profile -> Html msg
viewProfile maybeProfile =
    case maybeProfile of
        Just profile ->
            H.ul []
                [ H.li [] [ H.text ("First name: " ++ profile.firstName) ]
                , H.li [] [ H.text ("Last name: " ++ profile.lastName) ]
                ]

        Nothing ->
            H.p [] [ H.text "No profile." ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- INTERNAL


fetchProfile : Environment -> Cmd Msg
fetchProfile env =
    WebshopService.getProfile env
        |> Http.toTask
        |> Task.attempt ReceiveProfile


updateProfile : Environment -> String -> String -> Cmd Msg
updateProfile env firstName lastName =
    WebshopService.updateProfile env firstName lastName
        |> Http.toTask
        |> Task.attempt ReceiveUpdateProfile
