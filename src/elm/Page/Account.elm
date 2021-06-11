module Page.Account exposing (EditSection(..), Model, Msg(..), init, setEditSection, subscriptions, update, view)

import Base exposing (AppInfo)
import Browser.Dom as Dom
import Environment exposing (Environment)
import Fragment.Icon as Icon
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Attributes.Autocomplete exposing (ContactCompletion(..))
import Html.Events as E
import Html.Extra
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Error(..))
import Notification
import PageUpdater exposing (PageUpdater)
import Route exposing (Route)
import Service.FirebaseAuth as FirebaseAuth
import Service.Misc as MiscService exposing (Profile, SignInMethod, SignInProvider(..))
import Service.Webshop as WebshopService
import Shared exposing (Shared)
import Task
import Time exposing (Month(..), ZoneName(..))
import Ui.Button as B
import Ui.InlineButtonLink
import Ui.Input.EditSection as EditSection
import Ui.Input.MaskedText as MaskedInput
import Ui.Input.Text as Text
import Ui.Message exposing (error)
import Ui.Section
import Ui.TravelCardText
import Url.Builder as Url
import Util.Maybe
import Util.PhoneNumber
import Util.TravelCard
import Util.Validation as Validation exposing (FormError, ValidationErrors)
import Validate exposing (Valid)


type EditSection
    = TravelCardSection
    | NameSection
    | EmailSection
    | PhoneSection


type FieldName
    = TravelCard
    | Email
    | PhoneInput
    | FirstName
    | LastName
    | NameFields


type Msg
    = OnEnterPage
    | UpdateFirstName String
    | UpdateLastName String
    | UpdateEmail String
    | UpdatePhone String
    | InputTravelCard String
    | StateTravelCard MaskedInput.State
    | ReceiveUpdateProfile (List FieldName) (Result Http.Error ())
    | RemoveTravelCard
    | Logout
    | ProfileChange (Maybe Profile)
    | SaveNames
    | SaveEmail
    | SavePhone
    | ResetPassword String
    | RequestResetPassword
    | SaveTravelCard
    | SetEditSection (Maybe EditSection) (Maybe String)
    | LoadingEditSection (Maybe EditSection)
    | ClearValidationError
    | FocusItem String
    | ResetState
    | NoOp


type alias Model =
    { firstName : String
    , lastName : String
    , email : String
    , phone : String
    , travelCard : String
    , travelCardState : MaskedInput.State
    , profile : Maybe Profile
    , editSection : Maybe EditSection
    , loadingEditSection : Maybe EditSection
    , validationErrors : ValidationErrors FieldName
    }


init : ( Model, Cmd Msg )
init =
    ( { firstName = ""
      , lastName = ""
      , email = ""
      , phone = ""
      , travelCard = ""
      , travelCardState = MaskedInput.initState
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
        OnEnterPage ->
            PageUpdater.init model
                |> (Just "Min profil"
                        |> GA.SetTitle
                        |> PageUpdater.addGlobalAction
                   )

        ResetState ->
            let
                mapProfileWithDefault s =
                    model.profile |> Maybe.map s |> Maybe.withDefault ""
            in
                PageUpdater.init
                    { model
                        | editSection = Nothing
                        , firstName = mapProfileWithDefault .firstName
                        , lastName = mapProfileWithDefault .lastName
                        , email = mapProfileWithDefault .email
                        , phone = mapProfileWithDefault .phone
                        , travelCardState = MaskedInput.initState
                        , travelCard = model.profile |> Util.Maybe.flatMap .travelCard |> Maybe.map (.id >> String.fromInt) |> Maybe.withDefault ""
                        , loadingEditSection = Nothing
                        , validationErrors = []
                    }

        UpdateFirstName value ->
            PageUpdater.init { model | firstName = value }

        UpdateLastName value ->
            PageUpdater.init { model | lastName = value }

        UpdateEmail value ->
            PageUpdater.init { model | email = value }

        UpdatePhone value ->
            PageUpdater.init { model | phone = value }

        ResetPassword email ->
            PageUpdater.init model
                |> ("E-post med for å sette nytt passord er sendt til "
                        ++ email
                        |> H.text
                        |> Ui.Message.Valid
                        |> Ui.Message.message
                        |> (\s -> Notification.setContent s Notification.init)
                        |> GA.ShowNotification
                        |> PageUpdater.addGlobalAction
                   )

        RequestResetPassword ->
            case model.profile of
                Just profile ->
                    PageUpdater.fromPair ( model, resetUsingEmail profile.email )

                Nothing ->
                    PageUpdater.init model

        InputTravelCard value ->
            PageUpdater.init
                { model
                    | travelCard = value
                    , validationErrors = Validation.remove TravelCard model.validationErrors
                }

        StateTravelCard state ->
            PageUpdater.init
                { model | travelCardState = state }

        SaveNames ->
            PageUpdater.fromPair
                ( { model
                    | loadingEditSection = Just NameSection
                    , validationErrors = Validation.removeAll [ FirstName, LastName, NameFields ] model.validationErrors
                  }
                , updateProfile env model.firstName model.lastName
                )

        SavePhone ->
            let
                modelWithCode =
                    { model
                        | phone = Util.PhoneNumber.withCountryCode model.phone
                    }
            in
                case validatePhone .phone modelWithCode of
                    Ok _ ->
                        PageUpdater.fromPair
                            ( { model
                                | loadingEditSection = Just PhoneSection
                                , validationErrors = Validation.remove PhoneInput model.validationErrors
                              }
                            , updatePhone env modelWithCode.phone
                            )

                    Err errors ->
                        PageUpdater.init { model | validationErrors = errors }

        ReceiveUpdateProfile field result ->
            case result of
                Ok () ->
                    PageUpdater.init
                        { model
                            | loadingEditSection = Nothing
                            , editSection = Nothing
                            , validationErrors = Validation.removeAll field model.validationErrors
                        }

                Err error ->
                    PageUpdater.init
                        { model
                            | loadingEditSection = Nothing
                            , validationErrors = Validation.add field (Util.TravelCard.serverErrorToString WebshopService.travelCardErrorDecoder error) model.validationErrors
                        }

        ProfileChange (Just profile) ->
            PageUpdater.init
                { model
                    | profile = Just profile
                    , firstName = profile.firstName
                    , lastName = profile.lastName
                    , email = profile.email
                    , phone = profile.phone
                    , travelCard = Maybe.withDefault "" (Maybe.map (.id >> String.fromInt) profile.travelCard)
                    , validationErrors = []
                }

        ProfileChange Nothing ->
            PageUpdater.init { model | profile = Nothing }

        RemoveTravelCard ->
            PageUpdater.fromPair
                ( { model
                    | loadingEditSection = Just TravelCardSection
                    , validationErrors = Validation.remove TravelCard model.validationErrors
                  }
                , removeTravelCard env model.travelCard
                )

        SaveEmail ->
            case validateEmail .email model of
                Ok _ ->
                    PageUpdater.fromPair
                        ( { model
                            | loadingEditSection = Just EmailSection
                            , validationErrors = Validation.remove Email model.validationErrors
                          }
                        , updateEmail env model.email
                        )

                Err errors ->
                    PageUpdater.init { model | validationErrors = errors }

        SaveTravelCard ->
            case validateTravelCard model of
                Ok _ ->
                    PageUpdater.fromPair
                        ( { model
                            | loadingEditSection = Just TravelCardSection
                            , validationErrors = Validation.remove TravelCard model.validationErrors
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
            PageUpdater.init
                { model
                    | firstName = model.firstName
                    , lastName = model.lastName
                    , email = model.email
                    , editSection = Nothing
                    , validationErrors = []
                }
                |> PageUpdater.addGlobalAction GA.Logout

        ClearValidationError ->
            PageUpdater.init { model | validationErrors = [] }

        FocusItem id ->
            PageUpdater.fromPair ( model, focusBox <| Just id )

        NoOp ->
            PageUpdater.init model


validateEmail : (Model -> String) -> Model -> Result (List (FormError FieldName)) (Valid Model)
validateEmail sel model =
    if String.isEmpty model.email then
        Validation.validate Validation.void model

    else
        Validation.validate (Validation.emailValidator Email sel) model


validatePhone : (Model -> String) -> Model -> Result (List (FormError FieldName)) (Valid Model)
validatePhone sel =
    Validation.validate (Validation.phoneValidator PhoneInput sel)


validateTravelCard : Model -> Result (List (FormError FieldName)) (Valid Model)
validateTravelCard =
    Validation.validate (Validation.travelCardValidator TravelCard .travelCard)


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
viewSidebar model =
    let
        phoneNumber =
            model.profile
                |> Maybe.map .phone
                |> Maybe.withDefault "<Telefonnummer her>"

        subject =
            "Slett AtB-profilen min"

        body =
            ("Jeg ønsker at min AtB-profil med all tilhørende informasjon slettes fra nettbutikk og AtB-systemene. Profilen min er tilknyttet telefonnummer: "
                ++ phoneNumber
            )
                ++ """

                Jeg forstår at sletting av min AtB-profil innebærer følgende:
                
                - Eventuelle fortsatt gyldige eller fremtidige billetter på min profil vil slettes.
                - Jeg får ikke lenger tilgang til billetthistorikk eller kvitteringer.
                - Jeg må legge ved kopi av gyldig identifikasjon i denne e-posten, slik at kundeservice kan verifisere at jeg er eier av profilen.
                """

        deleteLink =
            "mailto:kundeservice@atb.no"
                ++ Url.toQuery
                    [ Url.string "body" body
                    , Url.string "subject" subject
                    ]
    in
        Ui.Section.view
            [ B.init "Logg ut"
                |> B.setIcon (Just Icon.logout)
                |> B.setOnClick (Just Logout)
                |> B.tertiary
            , B.init "Slett konto"
                |> B.setIcon (Just Icon.delete)
                |> B.setElement H.a
                |> B.setAttributes [ A.href deleteLink, A.title "Send epost til kundeservice med telefonnummer for å få slettet konto." ]
                |> B.primary B.Primary_destructive
            ]


viewMain : Model -> Html Msg
viewMain model =
    H.div [ A.class "main" ]
        [ Ui.Section.view
            (case model.profile of
                Just profile ->
                    [ viewProfile model profile
                    , viewSignIn model profile
                    , viewTravelCard model profile
                    , Ui.Section.viewGroup "Personvern"
                        [ Ui.Section.viewPaddedItem
                            [ H.p [] [ H.a [ A.href "https://beta.atb.no/private-policy" ] [ H.text "Les vår personvernerklæring" ] ]
                            ]
                        ]
                    ]

                Nothing ->
                    [ H.p [] [ H.text "No profile." ] ]
            )
        ]


viewProfile : Model -> Profile -> Html Msg
viewProfile model profile =
    let
        onSave =
            Just SaveNames

        onCancel =
            Just <| SetEditSection Nothing Nothing

        disabledButtons =
            model.loadingEditSection == Just EmailSection
    in
        Ui.Section.viewGroup "Profilinformasjon"
            [ Html.Extra.viewMaybe Ui.Message.error (Validation.select NameFields model.validationErrors)
            , EditSection.init
                "Administrer profilinformasjon"
                |> EditSection.setEditButtonType
                    ( "Endre navn", Icon.edit )
                |> EditSection.setOnSave onSave
                |> EditSection.setOnEdit (Just <| SetEditSection (Just NameSection) (Just "firstname"))
                |> EditSection.setInEditMode (fieldInEditMode model.editSection NameSection)
                |> EditSection.setIcon (Just Icon.profileLarge)
                |> EditSection.setButtonGroup
                    (Just <|
                        EditSection.cancelConfirmGroup
                            { onCancel = onCancel
                            , disabled = disabledButtons
                            }
                    )
                |> EditSection.editSection
                    (\inEditMode ->
                        EditSection.horizontalGroup
                            (if inEditMode then
                                [ Text.init "firstname"
                                    |> Text.setTitle (Just "Fornavn")
                                    |> Text.setError (Validation.select FirstName model.validationErrors)
                                    |> Text.setOnInput (Just <| UpdateFirstName)
                                    |> Text.setPlaceholder "Legg til et fornavn"
                                    |> Text.setValue (Just model.firstName)
                                    |> Text.view
                                , Text.init "lastname"
                                    |> Text.setTitle (Just "Etternavn")
                                    |> Text.setError (Validation.select LastName model.validationErrors)
                                    |> Text.setOnInput (Just <| UpdateLastName)
                                    |> Text.setPlaceholder "Legg til et etternavn"
                                    |> Text.setValue (Just model.lastName)
                                    |> Text.view
                                ]

                             else
                                [ Ui.Section.viewLabelItem "Fornavn" [ viewField identity profile.firstName ]
                                , Ui.Section.viewLabelItem "Etternavn" [ viewField identity profile.lastName ]
                                ]
                            )
                    )
            , viewEmailAddress model profile
            , viewPhoneNumber model profile
            ]


viewSignIn : Model -> Profile -> Html Msg
viewSignIn model profile =
    Ui.Section.viewGroup "Innloggingsmetode" <|
        List.map (viewSignInMethod model) profile.signInMethods


viewSignInMethod : Model -> SignInMethod -> Html Msg
viewSignInMethod _ method =
    case method.provider of
        Phone ->
            Ui.Section.viewWithIcon Icon.signInMethodLarge
                [ Ui.Section.viewLabelItem "Engangspassord"
                    [ H.text "Engangspassord på SMS til "
                    , viewField Util.PhoneNumber.format method.uid
                    ]
                ]

        Password ->
            Ui.Section.viewWithIcon Icon.signInMethodLarge
                [ Ui.Section.viewLabelItem "E-post og passord"
                    [ H.text <| "Logg på med e-post " ++ method.uid ++ ". "
                    , Ui.InlineButtonLink.view
                        [ E.onClick RequestResetPassword
                        , A.title <| "Send forespørsel for nytt passord (sender epost til " ++ method.uid ++ ")"
                        ]
                        [ H.text "Lag nytt passord" ]
                    ]
                ]

        _ ->
            Html.Extra.nothing


viewPhoneNumber : Model -> Profile -> Html Msg
viewPhoneNumber model profile =
    let
        onSave =
            Just SavePhone

        onCancel =
            Just <| SetEditSection Nothing Nothing

        hasEmail =
            profile.phone /= ""

        disabledButtons =
            model.loadingEditSection == Just PhoneSection
    in
        if hasProvider profile Phone then
            Html.Extra.nothing

        else
            EditSection.init "Administrer telefonnummer"
                |> EditSection.setEditButtonType
                    (if hasEmail then
                        ( "Endre telefonnummer", Icon.edit )

                     else
                        ( "Legg til telefonnummer", Icon.edit )
                    )
                |> EditSection.setOnSave onSave
                |> EditSection.setOnEdit (Just <| SetEditSection (Just PhoneSection) (Just "phone"))
                |> EditSection.setInEditMode (fieldInEditMode model.editSection PhoneSection)
                |> EditSection.setIcon (Just Icon.phoneLarge)
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
                                [ Text.init "phone"
                                    |> Text.setTitle (Just "Telefonnummer")
                                    |> Text.setError (Validation.select PhoneInput model.validationErrors)
                                    |> Text.setOnInput (Just <| UpdatePhone)
                                    |> Text.setPlaceholder "Legg til et telefonnummer"
                                    |> Text.setValue (Just model.phone)
                                    |> Text.setType "tel"
                                    |> Text.setError (Validation.select PhoneInput model.validationErrors)
                                    |> Text.view
                                ]

                        else
                            [ Ui.Section.viewLabelItem "Telefonnummer" [ viewField identity profile.phone ]
                            , model.validationErrors
                                |> Validation.select PhoneInput
                                |> Html.Extra.viewMaybe Ui.Message.error
                            ]
                    )


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
        if hasProvider profile Password then
            Html.Extra.nothing

        else
            EditSection.init "Administrer e-post"
                |> EditSection.setEditButtonType
                    (if hasEmail then
                        ( "Endre epostadresse", Icon.edit )

                     else
                        ( "Legg til epostadresse", Icon.edit )
                    )
                |> EditSection.setOnSave onSave
                |> EditSection.setOnEdit (Just <| SetEditSection (Just EmailSection) (Just "email"))
                |> EditSection.setInEditMode (fieldInEditMode model.editSection EmailSection)
                |> EditSection.setIcon (Just Icon.emailLarge)
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
                                    |> Text.setError (Validation.select Email model.validationErrors)
                                    |> Text.setOnInput (Just <| UpdateEmail)
                                    |> Text.setPlaceholder "Legg til en e-postadresse"
                                    |> Text.setValue (Just model.email)
                                    |> Text.setType "email"
                                    |> Text.view
                                ]

                        else
                            [ Ui.Section.viewLabelItem "E-post" [ viewField identity profile.email ]
                            , model.validationErrors
                                |> Validation.select Email
                                |> Html.Extra.viewMaybe Ui.Message.error
                            ]
                    )


hasProvider : Profile -> SignInProvider -> Bool
hasProvider profile p =
    profile.signInMethods
        |> List.filter (.provider >> (==) p)
        |> List.isEmpty
        |> not


setEditSection : Maybe EditSection -> Model -> Model
setEditSection editSection model =
    { model | editSection = editSection }


fieldInEditMode : Maybe EditSection -> EditSection -> Bool
fieldInEditMode state actual =
    state == Just actual


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
        Ui.Section.viewGroup "Billettbærere"
            [ EditSection.init "Administrer t:kort"
                |> EditSection.setEditButtonType
                    (if hasTravelCard then
                        ( "Fjern t:kort", Icon.delete )

                     else
                        ( "Legg til t:kort", Icon.edit )
                    )
                |> EditSection.setIcon (Just Icon.ticketLarge)
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
                                [ MaskedInput.init "tkort" InputTravelCard StateTravelCard
                                    |> MaskedInput.setTitle (Just "t:kortnummer (16-siffer)")
                                    |> MaskedInput.setError (Validation.select TravelCard model.validationErrors)
                                    |> MaskedInput.setPlaceholder "Skriv inn t:kortnummer"
                                    |> MaskedInput.setPattern "#### #### ########"
                                    |> MaskedInput.setAttributes
                                        [ A.autofocus True
                                        , A.attribute "inputmode" "numeric"
                                        ]
                                    |> MaskedInput.view model.travelCardState model.travelCard
                                ]

                        else
                            [ Ui.Section.viewLabelItem "t:kortnummer"
                                [ case profile.travelCard of
                                    Just travelCard ->
                                        travelCard
                                            |> .id
                                            |> Ui.TravelCardText.view

                                    Nothing ->
                                        H.div [ A.class "pageAccount__noTravelCard" ]
                                            [ Icon.warningColor
                                            , H.p []
                                                [ H.text "Du har ingen billettbærere! Last ned appen vår eller "
                                                , Ui.InlineButtonLink.view
                                                    [ E.onClick <| SetEditSection (Just TravelCardSection) (Just "tkort")
                                                    ]
                                                    [ H.text "legg til et t:kort" ]
                                                , H.text ". Har du ikke t:kort kan du "
                                                , H.a
                                                    [ A.href "https://www.atb.no/bestill-tkort/"
                                                    , A.target "_blank"
                                                    , A.title "Gå til skjema for å bestille nytt t:kort sendt til deg (åpner ny side)."
                                                    ]
                                                    [ H.text "bestille her (åpner ny side)" ]
                                                , H.text "."
                                                ]
                                            ]
                                ]
                            , model.validationErrors
                                |> Validation.select TravelCard
                                |> Html.Extra.viewMaybe Ui.Message.error
                            ]
                    )
            ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ MiscService.onProfileChange ProfileChange
        ]



-- INTERNAL


{-| Field is valid if it is neither an empty string or an underscore.
-}
hasField : String -> Bool
hasField x =
    x /= "" && x /= "_"


{-| Show the field as normal if it is valid, otherwise say that it's not filled out.
-}
viewField : (String -> String) -> String -> Html msg
viewField fn x =
    if hasField x then
        H.span [] [ H.text <| fn x ]

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
        |> Task.attempt (ReceiveUpdateProfile [ NameFields ])


updateEmail : Environment -> String -> Cmd Msg
updateEmail env email =
    WebshopService.updateEmail env email
        |> Http.toTask
        |> Task.attempt (ReceiveUpdateProfile [ Email ])


updatePhone : Environment -> String -> Cmd Msg
updatePhone env phone =
    WebshopService.updatePhone env phone
        |> Http.toTask
        |> Task.attempt (ReceiveUpdateProfile [ PhoneInput ])


updateTravelCard : Environment -> String -> Cmd Msg
updateTravelCard env travelCard =
    travelCard
        |> WebshopService.addTravelCard env
        |> Http.toTask
        |> Task.attempt (ReceiveUpdateProfile [ TravelCard ])


removeTravelCard : Environment -> String -> Cmd Msg
removeTravelCard env travelCard =
    WebshopService.deleteTravelCard env travelCard
        |> Http.toTask
        |> Task.attempt (ReceiveUpdateProfile [ TravelCard ])


resetUsingEmail : String -> Cmd Msg
resetUsingEmail email =
    FirebaseAuth.resetPassword email
