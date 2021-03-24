module Page.Account exposing (Model, Msg(..), init, subscriptions, update, view)

import Base exposing (AppInfo)
import Environment exposing (Environment)
import Fragment.Icon as Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Http
import PageUpdater exposing (PageUpdater)
import Route exposing (Route)
import Service.Misc as MiscService exposing (Profile)
import Service.Webshop as WebshopService
import Shared exposing (Shared)
import Task


type Msg
    = UpdateFirstName String
    | UpdateLastName String
    | UpdateProfile
    | ReceiveUpdateProfile (Result Http.Error ())
    | EditName
    | EditPhoneNumber
    | RemoveTravelCard
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


view : Environment -> AppInfo -> Shared -> Model -> Maybe Route -> Html Msg
view _ _ _ model _ =
    H.div [ A.class "page-account" ]
        [ viewMain model
        , H.div [] [ viewSidebar model ]
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
                    [ H.div [ A.class "section-header" ] [ H.text "Min konto" ]
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
        H.div [ A.class "two-col" ]
            [ H.div []
                [ H.label [] [ H.text "Fornavn" ]
                , viewField profile.firstName
                ]
            , H.div []
                [ H.label [] [ H.text "Etternavn" ]
                , viewField profile.lastName
                ]
            ]

    else
        H.text ""


viewPhoneNumber : Profile -> Html msg
viewPhoneNumber profile =
    if hasField profile.phone then
        H.div []
            [ H.label [] [ H.text "Telefonnummer" ]
            , viewField profile.phone
            ]

    else
        H.text ""


viewEmailAddress : Profile -> Html msg
viewEmailAddress profile =
    if hasField profile.email then
        H.div []
            [ H.label [] [ H.text "E-postadresse" ]
            , viewField profile.email
            ]

    else
        H.text ""


viewTravelCard : Profile -> Html msg
viewTravelCard profile =
    case profile.travelCard of
        Just travelCard ->
            H.div []
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
