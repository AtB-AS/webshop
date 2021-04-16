module Page.Account exposing (Model, Msg(..), init, subscriptions, update, view)

import Base exposing (AppInfo)
import Environment exposing (Environment)
import Fragment.Icon as Icon
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A exposing (disabled)
import Html.Events as E exposing (onClick)
import Http
import PageUpdater exposing (PageUpdater)
import Route exposing (Route)
import Service.Misc as MiscService exposing (Profile)
import Service.Webshop as WebshopService
import Shared exposing (Shared)
import Task
import Ui.Button
import Ui.Section


type Msg
    = UpdateFirstName String
    | UpdateLastName String
    | UpdateProfile
    | ReceiveUpdateProfile (Result Http.Error ())
    | EditName
    | EditPhoneNumber
    | RemoveTravelCard
    | Logout
    | DeleteAccount
    | ProfileChange (Maybe Profile)


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
                    PageUpdater.init { model | updating = False }

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
                    | profile = Just profile
                    , firstName = profile.firstName
                    , lastName = profile.lastName
                }

        ProfileChange Nothing ->
            PageUpdater.init { model | profile = Nothing }

        Logout ->
            PageUpdater.init model
                |> PageUpdater.addGlobalAction GA.Logout


view : Environment -> AppInfo -> Shared -> Model -> Maybe Route -> Html Msg
view _ _ _ model _ =
    H.div [ A.class "page" ]
        [ viewMain model
        , H.div [] [ viewSidebar model ]
        ]


viewSidebar : Model -> Html Msg
viewSidebar model =
    Ui.Section.section
        [ Ui.Button.tertiary
            { text = "Logg ut"
            , disabled = False
            , icon = Just Icon.logout
            , onClick = Just Logout
            }
        , Ui.Button.tertiary
            { text = "Slett konto"
            , disabled = True
            , icon = Just Icon.delete
            , onClick = Nothing
            }
        ]


viewMain : Model -> Html Msg
viewMain model =
    H.div [ A.class "main" ]
        [ Ui.Section.section
            (case model.profile of
                Just profile ->
                    [ Ui.Section.sectionHeader "Min konto"
                    , viewProfile profile
                    , viewTravelCard profile
                    , viewPhoneNumber profile
                    , viewEmailAddress profile
                    ]

                Nothing ->
                    [ H.p [] [ H.text "No profile." ] ]
            )
        ]


viewProfile : Profile -> Html msg
viewProfile profile =
    if hasField profile.firstName || hasField profile.lastName then
        Ui.Section.sectionGenericItem
            [ H.div [ A.class "two-col" ]
                [ H.div []
                    [ H.label [] [ H.text "Fornavn" ]
                    , viewField profile.firstName
                    ]
                , H.div []
                    [ H.label [] [ H.text "Etternavn" ]
                    , viewField profile.lastName
                    ]
                ]
            ]

    else
        H.text ""


viewPhoneNumber : Profile -> Html msg
viewPhoneNumber profile =
    if hasField profile.phone then
        Ui.Section.sectionGenericItem
            [ H.label [] [ H.text "Telefonnummer" ]
            , viewField profile.phone
            ]

    else
        H.text ""


viewEmailAddress : Profile -> Html msg
viewEmailAddress profile =
    if hasField profile.email then
        Ui.Section.sectionGenericItem
            [ H.label [] [ H.text "E-postadresse" ]
            , viewField profile.email
            ]

    else
        H.text ""


viewTravelCard : Profile -> Html msg
viewTravelCard profile =
    case profile.travelCard of
        Just travelCard ->
            Ui.Section.sectionGenericItem
                [ H.label [] [ H.text "t:kort" ]
                , viewField <| String.fromInt <| travelCard.id
                ]

        Nothing ->
            H.text ""


subscriptions : Model -> Sub Msg
subscriptions _ =
    MiscService.onProfileChange ProfileChange



-- INTERNAL


{-| Field is valid if it is neither an empty string or an underscore.
-}
hasField : String -> Bool
hasField x =
    x /= "" && x /= "_"


{-| Show the field as normal if it is valid, otherwise say that it's not filled out.
-}
viewField : String -> Html msg
viewField x =
    if hasField x then
        H.span [] [ H.text x ]

    else
        H.span
            [ A.style "opacity" "0.5"
            , A.style "font-style" "italic"
            ]
            [ H.text "(ikke utfylt)" ]


updateProfile : Environment -> String -> String -> Cmd Msg
updateProfile env firstName lastName =
    WebshopService.updateProfile env firstName lastName
        |> Http.toTask
        |> Task.attempt ReceiveUpdateProfile
