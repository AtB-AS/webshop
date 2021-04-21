module Page.Account exposing (Model, Msg(..), init, subscriptions, update, view)

import Base exposing (AppInfo)
import Browser.Dom as Dom
import Environment exposing (Environment)
import Fragment.Icon as Icon
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
import Http exposing (Error(..))
import Json.Decode exposing (Error(..))
import List.Extra
import PageUpdater exposing (PageUpdater)
import Route exposing (Route)
import Service.Misc as MiscService exposing (Profile)
import Service.Webshop as WebshopService
import Shared exposing (Shared)
import Task
import Time exposing (Month(..))
import Ui.Button as B
import Ui.Input.EditSection as EditSection
import Ui.Input.Text as Text
import Ui.Section
import Validate as Validate


type EditSection
    = TravelCardSection
    | NameSection


type FieldName
    = TravelCard


type alias FormError =
    ( FieldName, String )


type Msg
    = UpdateFirstName String
    | UpdateLastName String
    | UpdateTravelCard String
    | UpdateProfile
    | ReceiveUpdateProfile (Result Http.Error ())
    | ReceiveUpdateTravelCard (Result Http.Error ())
    | EditName
    | EditPhoneNumber
    | RemoveTravelCard
    | Logout
    | DeleteAccount
    | ProfileChange (Maybe Profile)
    | ValidateTravelCard
    | SaveTravelCard
    | SetEditSection (Maybe EditSection) (Maybe String)
    | LoadingEditSection (Maybe EditSection)
    | ClearValidationError
    | FocusItem String
    | NoOp


type alias Model =
    { firstName : String
    , lastName : String
    , travelCard : String
    , profile : Maybe Profile
    , editSection : Maybe EditSection
    , loadingEditSection : Maybe EditSection
    , validationErrors : List FormError
    }


init : ( Model, Cmd Msg )
init =
    ( { firstName = ""
      , lastName = ""
      , travelCard = ""
      , profile = Nothing
      , editSection = Nothing
      , loadingEditSection = Nothing
      , validationErrors = []
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

        UpdateTravelCard value ->
            PageUpdater.init
                { model
                    | travelCard = value
                    , validationErrors = clearValidationError TravelCard model.validationErrors
                }

        UpdateProfile ->
            PageUpdater.fromPair
                ( { model | loadingEditSection = Just NameSection }
                , updateProfile env model.firstName model.lastName
                )

        ReceiveUpdateProfile result ->
            case result of
                Ok () ->
                    PageUpdater.init { model | loadingEditSection = Nothing }

                Err _ ->
                    PageUpdater.init { model | loadingEditSection = Nothing }

        ReceiveUpdateTravelCard result ->
            case result of
                Ok () ->
                    PageUpdater.init { model | loadingEditSection = Nothing }

                Err error ->
                    PageUpdater.init
                        { model
                            | loadingEditSection = Nothing
                            , validationErrors = addValidationError ( TravelCard, errorToString error ) model.validationErrors
                        }

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
                    , travelCard = Maybe.withDefault "" (Maybe.map (.id >> String.fromInt) profile.travelCard)
                    , validationErrors = []
                }

        ProfileChange Nothing ->
            PageUpdater.init { model | profile = Nothing }

        SaveTravelCard ->
            case Validate.validate travelCardValidator model of
                Ok _ ->
                    PageUpdater.fromPair
                        ( { model
                            | loadingEditSection = Just TravelCardSection
                            , validationErrors = clearValidationError TravelCard model.validationErrors
                          }
                        , updateTravelCard env model.travelCard
                        )

                Err errors ->
                    PageUpdater.init { model | validationErrors = errors }

        ValidateTravelCard ->
            case Validate.validate travelCardValidator model of
                Ok _ ->
                    PageUpdater.init model

                Err errors ->
                    PageUpdater.init { model | validationErrors = errors }

        SetEditSection (Just section) focusId ->
            PageUpdater.init { model | editSection = Just section, validationErrors = [] }
                |> PageUpdater.addCmd (focusBox focusId)

        SetEditSection Nothing focusId ->
            case model.profile of
                Nothing ->
                    PageUpdater.init model

                Just profile ->
                    PageUpdater.fromPair
                        ( { model
                            | editSection = Nothing
                            , validationErrors = []
                            , firstName = profile.firstName
                            , lastName = profile.lastName
                            , travelCard = Maybe.withDefault "" (Maybe.map (.id >> String.fromInt) profile.travelCard)
                          }
                        , focusBox focusId
                        )

        LoadingEditSection section ->
            PageUpdater.init { model | loadingEditSection = section }

        Logout ->
            PageUpdater.init model
                |> PageUpdater.addGlobalAction GA.Logout

        ClearValidationError ->
            PageUpdater.init { model | validationErrors = [] }

        FocusItem id ->
            PageUpdater.fromPair ( model, focusBox <| Just id )

        NoOp ->
            PageUpdater.init model


selectValidationError : FieldName -> List FormError -> Maybe String
selectValidationError fieldName =
    List.Extra.find (Tuple.first >> (==) fieldName) >> Maybe.map Tuple.second


clearValidationError : FieldName -> List FormError -> List FormError
clearValidationError fieldName =
    List.filter (Tuple.first >> (/=) fieldName)


addValidationError : FormError -> List FormError -> List FormError
addValidationError formError =
    (::) formError


focusBox : Maybe String -> Cmd Msg
focusBox id =
    id
        |> Maybe.map (\i -> Task.attempt (\_ -> NoOp) (Dom.focus i))
        |> Maybe.withDefault Cmd.none


view : Environment -> AppInfo -> Shared -> Model -> Maybe Route -> Html Msg
view _ _ _ model _ =
    H.div [ A.class "page" ]
        [ viewMain model
        , H.div [] [ viewSidebar model ]
        ]


viewSidebar : Model -> Html Msg
viewSidebar _ =
    Ui.Section.section
        [ B.init "Logg ut"
            |> B.setIcon (Just Icon.logout)
            |> B.setOnClick (Just Logout)
            |> B.tertiary
        , B.init "Slett konto"
            |> B.setIcon (Just Icon.delete)
            |> B.setDisabled True
            |> B.setOnClick (Just Logout)
            |> B.tertiary
        ]


viewMain : Model -> Html Msg
viewMain model =
    H.div [ A.class "main" ]
        [ Ui.Section.section
            (case model.profile of
                Just profile ->
                    [ Ui.Section.sectionHeader "Min konto"
                    , viewProfile profile
                    , viewPhoneNumber profile
                    , viewTravelCard model profile
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
    Ui.Section.labelItem "Telefonnummer" [ viewField profile.phone ]


viewEmailAddress : Profile -> Html msg
viewEmailAddress profile =
    Ui.Section.labelItem "E-postadresse" [ viewField profile.email ]


fieldInEditMode : Maybe EditSection -> EditSection -> Bool
fieldInEditMode state actual =
    state == Just actual


travelCardValidator : Validate.Validator FormError Model
travelCardValidator =
    Validate.firstError
        [ Validate.ifBlank .travelCard ( TravelCard, "t:kort id kan ikke være tomt." )
        , Validate.ifNotInt .travelCard (\_ -> ( TravelCard, "t:kort id må være et tall." ))
        ]


viewTravelCard : Model -> Profile -> Html Msg
viewTravelCard model profile =
    EditSection.init "Legg til eller fjern t:kort"
        |> EditSection.setEditText
            (case profile.travelCard of
                Just _ ->
                    "Endre / fjern t:kort"

                Nothing ->
                    "Legg til t:kort"
            )
        |> EditSection.setOnSave (Just SaveTravelCard)
        |> EditSection.setOnCancel (Just <| SetEditSection Nothing Nothing)
        |> EditSection.setOnEdit (Just <| SetEditSection (Just TravelCardSection) (Just "tkort"))
        |> EditSection.setInEditMode (fieldInEditMode model.editSection TravelCardSection)
        |> EditSection.editSection
            (\inEditMode ->
                if inEditMode then
                    EditSection.horizontalGroup
                        [ Text.init "tkort"
                            |> Text.setTitle (Just "t:kort")
                            |> Text.setError (selectValidationError TravelCard model.validationErrors)
                            |> Text.setOnInput (Just <| UpdateTravelCard)
                            |> Text.setOnBlur (Just <| ValidateTravelCard)
                            |> Text.setPlaceholder "Legg til et t:kort nå"
                            |> Text.setValue (Just model.travelCard)
                            |> Text.text
                        ]

                else
                    [ Ui.Section.labelItem "t:kort"
                        [ profile.travelCard
                            |> Maybe.map (.id >> String.fromInt)
                            |> Maybe.withDefault "Ikke lagt til"
                            |> H.text
                        ]
                    ]
            )


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


updateTravelCard : Environment -> String -> Cmd Msg
updateTravelCard env travelCard =
    WebshopService.addTravelCard env travelCard
        |> Http.toTask
        |> Task.attempt ReceiveUpdateTravelCard


errorToString : Http.Error -> String
errorToString error =
    case error of
        BadUrl url ->
            "The URL " ++ url ++ " was invalid"

        Timeout ->
            "Unable to reach the server, try again"

        NetworkError ->
            "Unable to reach the server, check your network connection"

        BadPayload errorMessage _ ->
            errorMessage

        BadStatus { status, body } ->
            case status.code of
                500 ->
                    "The server had a problem, try again later"

                400 ->
                    case WebshopService.travelCardErrorDecoder body of
                        Ok errorMessage ->
                            errorMessage

                        _ ->
                            "Verify your information and try again"

                _ ->
                    "Unknown error"
