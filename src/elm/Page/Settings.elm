module Page.Settings exposing (Model, Msg(..), init, subscriptions, update, view)

import Base exposing (AppInfo)
import Data.Webshop exposing (Profile)
import Environment exposing (Environment)
import Fragment.Button as Button
import Fragment.Icon as Icon
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Http
import PageUpdater exposing (PageUpdater)
import Route exposing (Route)
import Service.Misc as MiscService
import Service.Webshop as WebshopService
import Shared exposing (Shared)
import Task


type Msg
    = GetProfile
    | ReceiveProfile (Result Http.Error Profile)
    | UpdateFirstName String
    | UpdateLastName String
    | UpdateProfile
    | ReceiveUpdateProfile (Result Http.Error ())
    | EditName
    | EditPhoneNumber
    | RemoveTravelCard
    | DeleteAccount
    | ProfileChange (Maybe MiscService.Profile)


type alias Model =
    { firstName : String
    , lastName : String
    , profile : Maybe Profile
    , fireProfile : Maybe MiscService.Profile
    , updating : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { firstName = ""
      , lastName = ""
      , profile = Nothing
      , fireProfile = Nothing
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

        EditName ->
            PageUpdater.init model

        EditPhoneNumber ->
            PageUpdater.init model

        RemoveTravelCard ->
            PageUpdater.init model

        DeleteAccount ->
            PageUpdater.init model

        ProfileChange (Just profile) ->
            PageUpdater.init
                { model
                    | fireProfile = Just profile
                    , firstName = profile.firstName
                    , lastName = profile.lastName
                    , profile =
                        Just
                            { email = profile.email
                            , firstName = profile.firstName
                            , lastName = profile.lastName
                            , customerNumber = 123
                            }
                }

        ProfileChange Nothing ->
            PageUpdater.init { model | fireProfile = Nothing }


view : Environment -> AppInfo -> Shared -> Model -> Maybe Route -> Html Msg
view _ _ _ model _ =
    H.div [ A.class "settings" ]
        [ viewMain model
        , viewSidebar model
        ]


richActionButton : Bool -> Maybe msg -> Html msg -> Html msg
richActionButton active maybeAction content =
    let
        baseAttributes =
            [ A.classList
                [ ( "active", active )
                , ( "pseudo-button", maybeAction /= Nothing )
                , ( "pseudo-button-disabled", maybeAction == Nothing )
                ]
            ]

        attributes =
            case maybeAction of
                Just action ->
                    E.onClick action :: baseAttributes

                Nothing ->
                    baseAttributes
    in
        H.div attributes [ content ]


viewSidebar : Model -> Html Msg
viewSidebar model =
    H.div [ A.class "section-box" ]
        [ richActionButton False
            (Just EditName)
            (H.div [ A.style "display" "flex", A.style "width" "100%" ]
                [ H.span [ A.style "flex-grow" "1", A.style "margin" "0 8px", A.style "font-weight" "500" ] [ H.text "Endre navn" ]
                , Icon.rightArrow
                ]
            )
        , richActionButton False
            (Just EditPhoneNumber)
            (H.div [ A.style "display" "flex", A.style "width" "100%" ]
                [ H.span [ A.style "flex-grow" "1", A.style "margin" "0 8px", A.style "font-weight" "500" ] [ H.text "Endre telefonnummer" ]
                , Icon.rightArrow
                ]
            )
        , richActionButton False
            (Just RemoveTravelCard)
            (H.div [ A.style "display" "flex", A.style "width" "100%" ]
                [ H.span [ A.style "flex-grow" "1", A.style "margin" "0 8px", A.style "font-weight" "500" ] [ H.text "Fjern t:kort" ]
                , Icon.ticketRemove
                ]
            )
        , richActionButton False
            Nothing
            (H.div [ A.style "display" "flex", A.style "width" "100%" ]
                [ H.span [ A.style "flex-grow" "1", A.style "margin" "0 8px", A.style "font-weight" "500" ] [ H.text "Slett konto" ]
                , Icon.delete
                ]
            )
        ]


viewMain : Model -> Html Msg
viewMain model =
    H.div [ A.class "main" ]
        [ H.div [ A.class "section-box" ]
            (case model.profile of
                Just profile ->
                    [ viewProfile profile
                    , viewTravelCard profile
                    , viewAccountMetadata profile
                    ]

                Nothing ->
                    [ H.p [] [ H.text "No profile." ] ]
            )
        ]


viewProfile : Profile -> Html msg
viewProfile profile =
    H.div [] [ H.text <| profile.firstName ++ " " ++ profile.lastName ]


viewTravelCard : Profile -> Html msg
viewTravelCard profile =
    H.div [] [ H.text <| profile.email ]


viewAccountMetadata : Profile -> Html msg
viewAccountMetadata _ =
    H.div [] [ H.text "01.01.2021" ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    MiscService.onProfileChange ProfileChange



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
