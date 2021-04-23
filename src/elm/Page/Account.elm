module Page.Account exposing (Model, Msg(..), init, subscriptions, update, view)

import Base exposing (AppInfo)
import Browser.Dom as Dom
import Environment exposing (Environment)
import Fragment.Icon as Icon
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Attributes.Autocomplete exposing (ContactCompletion(..))
import Html.Extra
import Http exposing (Error(..))
import Json.Decode exposing (Error(..))
import List.Extra
import PageUpdater exposing (PageUpdater)
import Route exposing (Route)
import Service.Misc as MiscService exposing (Profile)
import Service.Webshop as WebshopService
import Shared exposing (Shared)
import Task
import Time exposing (Month(..), ZoneName(..))
import Ui.Button as B
import Ui.Input.EditSection as EditSection
import Ui.Input.Text as Text
import Ui.Message
import Ui.Section
import Validate as Validate


type EditSection
    = TravelCardSection
    | NameSection
    | EmailSection


type FieldName
    = TravelCard
    | Email
    | FirstName
    | LastName


type alias FormError =
    ( FieldName, String )


type Msg
    = UpdateFirstName String
    | UpdateLastName String
    | UpdateEmail String
    | UpdateTravelCard String
    | UpdateProfile
    | ReceiveUpdateProfile (List FieldName) (Result Http.Error ())
    | EditName
    | EditPhoneNumber
    | RemoveTravelCard
    | Logout
    | DeleteAccount
    | ProfileChange (Maybe Profile)
    | SaveEmail
    | SaveTravelCard
    | SetEditSection (Maybe EditSection) (Maybe String)
    | LoadingEditSection (Maybe EditSection)
    | ClearValidationError
    | FocusItem String
    | NoOp


type alias Model =
    { firstName : String
    , lastName : String
    , email : String
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
      , email = ""
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

        UpdateEmail value ->
            PageUpdater.init { model | email = value }

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

        ReceiveUpdateProfile field result ->
            case result of
                Ok () ->
                    PageUpdater.init { model | loadingEditSection = Nothing, editSection = Nothing }

                Err error ->
                    PageUpdater.init
                        { model
                            | loadingEditSection = Nothing
                            , validationErrors = addValidationError field (errorToString error) model.validationErrors
                        }

        EditName ->
            PageUpdater.init model

        EditPhoneNumber ->
            PageUpdater.init model

        DeleteAccount ->
            PageUpdater.init model

        ProfileChange (Just profile) ->
            PageUpdater.init
                { model
                    | profile = Just profile
                    , firstName = profile.firstName
                    , lastName = profile.lastName
                    , email = profile.email
                    , travelCard = Maybe.withDefault "" (Maybe.map (.id >> String.fromInt) profile.travelCard)
                    , validationErrors = []
                }

        ProfileChange Nothing ->
            PageUpdater.init { model | profile = Nothing }

        RemoveTravelCard ->
            PageUpdater.fromPair
                ( { model
                    | loadingEditSection = Just TravelCardSection
                    , validationErrors = clearValidationError TravelCard model.validationErrors
                  }
                , removeTravelCard env model.travelCard
                )

        SaveEmail ->
            case Validate.validate emailValidator model of
                Ok _ ->
                    PageUpdater.fromPair
                        ( { model
                            | loadingEditSection = Just EmailSection
                            , validationErrors = clearValidationError Email model.validationErrors
                          }
                        , updateEmail env model.email
                        )

                Err errors ->
                    PageUpdater.init { model | validationErrors = errors }

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
                            , email = profile.email
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


addValidationError : List FieldName -> String -> List FormError -> List FormError
addValidationError fields error =
    (++) (fields |> List.map (\a -> ( a, error )))


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
    Ui.Section.view
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
        [ Ui.Section.view
            (case model.profile of
                Just profile ->
                    [ Ui.Section.viewHeader "Min konto"
                    , viewProfile profile
                    , viewPhoneNumber profile
                    , viewTravelCard model profile
                    , viewEmailAddress model profile
                    ]

                Nothing ->
                    [ H.p [] [ H.text "No profile." ] ]
            )
        ]


viewProfile : Profile -> Html msg
viewProfile profile =
    H.div []
        [ Ui.Section.viewLabelItem "Fornavn" [ viewField profile.firstName ]
        , Ui.Section.viewLabelItem "Etternavn" [ viewField profile.lastName ]
        ]


viewPhoneNumber : Profile -> Html msg
viewPhoneNumber profile =
    Ui.Section.viewLabelItem "Telefonnummer" [ viewField profile.phone ]


emailValidator : Validate.Validator FormError Model
emailValidator =
    Validate.firstError
        [ Validate.ifInvalidEmail .email (\_ -> ( Email, "E-posten du har skrevet ser ikke ut til å være gyldig" ))
        ]


viewEmailAddress : Model -> Profile -> Html Msg
viewEmailAddress model profile =
    let
        onSave =
            Just SaveEmail

        onCancel =
            Just <| SetEditSection Nothing Nothing

        hasEmail =
            profile.email /= ""

        disabledButtons =
            model.loadingEditSection == Just EmailSection
    in
        EditSection.init "Administrer e-post"
            |> EditSection.setEditButtonType
                (if hasEmail then
                    ( "Rediger e-post", Icon.delete )

                 else
                    ( "Legg til e-post", Icon.edit )
                )
            |> EditSection.setOnSave onSave
            |> EditSection.setOnEdit (Just <| SetEditSection (Just EmailSection) (Just "email"))
            |> EditSection.setInEditMode (fieldInEditMode model.editSection EmailSection)
            |> EditSection.setButtonGroup
                (Just <|
                    EditSection.cancelConfirmGroup
                        { onCancel = onCancel
                        , disabled = disabledButtons
                        }
                )
            |> EditSection.editSection
                (\inEditMode ->
                    if inEditMode then
                        EditSection.horizontalGroup
                            [ Text.init "email"
                                |> Text.setTitle (Just "E-post")
                                |> Text.setError (selectValidationError Email model.validationErrors)
                                |> Text.setOnInput (Just <| UpdateEmail)
                                |> Text.setPlaceholder "Legg til et e-post"
                                |> Text.setValue (Just model.email)
                                |> Text.view
                            ]

                    else
                        [ Ui.Section.viewLabelItem "E-post" [ viewField profile.email ]
                        , model.validationErrors
                            |> selectValidationError Email
                            |> Html.Extra.viewMaybe Ui.Message.error
                        ]
                )


fieldInEditMode : Maybe EditSection -> EditSection -> Bool
fieldInEditMode state actual =
    state == Just actual


ifNotLength : Int -> (subject -> String) -> error -> Validate.Validator error subject
ifNotLength stringLength subjectToString error =
    Validate.ifTrue (\subject -> String.length (subjectToString subject) /= stringLength) error


travelCardValidator : Validate.Validator FormError Model
travelCardValidator =
    Validate.firstError
        [ Validate.ifBlank .travelCard ( TravelCard, "t:kort id kan ikke være tomt." )
        , ifNotLength 9 .travelCard ( TravelCard, "t:kort id ser ut til å være feil." )
        , Validate.ifNotInt .travelCard (\_ -> ( TravelCard, "t:kort id må være et tall." ))
        ]


viewTravelCard : Model -> Profile -> Html Msg
viewTravelCard model profile =
    let
        onSave =
            Just SaveTravelCard

        onRemove =
            Just RemoveTravelCard

        onCancel =
            Just <| SetEditSection Nothing Nothing

        hasTravelCard =
            profile.travelCard /= Nothing

        disabledButtons =
            model.loadingEditSection == Just TravelCardSection
    in
        EditSection.init "Administrer t:kort"
            |> EditSection.setEditButtonType
                (if hasTravelCard then
                    ( "Fjern t:kort", Icon.delete )

                 else
                    ( "Legg til t:kort", Icon.edit )
                )
            |> EditSection.setOnSave onSave
            |> EditSection.setOnEdit (Just <| SetEditSection (Just TravelCardSection) (Just "tkort"))
            |> EditSection.setInEditMode (fieldInEditMode model.editSection TravelCardSection)
            |> EditSection.setButtonGroup
                (if hasTravelCard then
                    Just <|
                        EditSection.destructiveGroup
                            { message = "Er du sikker på at du ønsker å fjerne dette t:kortet? Dette gjør at aktive billetter ikke lengre vil være tilgjengelig via kortet."
                            , onCancel = onCancel
                            , onDestroy = onRemove
                            , disabled = disabledButtons
                            }

                 else
                    Just <|
                        EditSection.cancelConfirmGroup
                            { onCancel = onCancel
                            , disabled = disabledButtons
                            }
                )
            |> EditSection.editSection
                (\inEditMode ->
                    if inEditMode && not hasTravelCard then
                        EditSection.horizontalGroup
                            [ Text.init "tkort"
                                |> Text.setTitle (Just "t:kort")
                                |> Text.setError (selectValidationError TravelCard model.validationErrors)
                                |> Text.setOnInput (Just <| UpdateTravelCard)
                                |> Text.setPlaceholder "Legg til et t:kort nå"
                                |> Text.setValue (Just model.travelCard)
                                |> Text.view
                            ]

                    else
                        [ Ui.Section.viewLabelItem "t:kort"
                            [ profile.travelCard
                                |> Maybe.map (.id >> String.fromInt)
                                |> Maybe.withDefault "Ingen t:kort lagt til"
                                |> H.text
                            ]
                        , model.validationErrors
                            |> selectValidationError TravelCard
                            |> Html.Extra.viewMaybe Ui.Message.error
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
        |> Task.attempt (ReceiveUpdateProfile [ FirstName, LastName ])


updateEmail : Environment -> String -> Cmd Msg
updateEmail env email =
    WebshopService.updateEmail env email
        |> Http.toTask
        |> Task.attempt (ReceiveUpdateProfile [ Email ])


updateTravelCard : Environment -> String -> Cmd Msg
updateTravelCard env travelCard =
    WebshopService.addTravelCard env travelCard
        |> Http.toTask
        |> Task.attempt (ReceiveUpdateProfile [ TravelCard ])


removeTravelCard : Environment -> String -> Cmd Msg
removeTravelCard env travelCard =
    WebshopService.deleteTravelCard env travelCard
        |> Http.toTask
        |> Task.attempt (ReceiveUpdateProfile [ TravelCard ])


errorToString : Http.Error -> String
errorToString error =
    case error of
        BadStatus { status, body } ->
            case status.code of
                500 ->
                    "Det skjedde en feil med tjenesten. Prøv igjen senere."

                400 ->
                    case WebshopService.travelCardErrorDecoder body of
                        Ok errorMessage ->
                            "Feilmelding fra tjenesten: " ++ errorMessage

                        _ ->
                            "Innsendt informasjon ser ut til å ikke stemme. Prøv igjen er du snill."

                _ ->
                    "Unknown error"

        _ ->
            "Fikk ikke kontakt med tjenesten. Sjekk om du er på nett og prøv igjen."
