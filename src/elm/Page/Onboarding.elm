module Page.Onboarding exposing (Model, Msg, init, subscriptions, update, view)

import Environment exposing (Environment)
import Fragment.Icon as Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Html.Extra
import Http exposing (Error(..))
import Json.Decode as Decode
import PageUpdater exposing (PageUpdater)
import Service.Misc as MiscService
import Service.Webshop as WebshopService
import Task
import Ui.Button as Button
import Ui.Input.Checkbox as Checkbox
import Ui.Input.MaskedText as MaskedInput
import Ui.Input.Text as TextInput
import Ui.LabelItem
import Ui.Message as Message
import Ui.PageHeader as PH
import Ui.Section as Section
import Util.Task
import Util.TravelCard
import Util.Validation as V exposing (FormError, ValidationErrors)
import Validate exposing (Valid)


type Msg
    = InputEmail String
    | InputFirstName String
    | InputLastName String
    | ToggleConsent1 Bool
    | ToggleConsent2 Bool
    | Register
    | ReceiveRegisterProfile (Result Http.Error ())
    | SkipRegister
    | InputTravelCard String
    | StateTravelCard MaskedInput.State
    | RegisterTravelCard
    | ReceiveRegisterTravelCard (Result Http.Error ())
    | Finish
    | NextStep
    | PrevStep


type Step
    = ProfileInfo
    | Consents
    | TravelCard
    | AppAdvert


type FieldName
    = TravelCardField
    | EmailField
    | RegisterForm


type alias Model =
    { token : String
    , firstName : String
    , lastName : String
    , email : String
    , phone : String
    , consent1 : Bool
    , consent2 : Bool
    , step : Step
    , travelCard : String
    , travelCardState : MaskedInput.State
    , travelCardSaved : Bool
    , validationErrors : ValidationErrors FieldName
    }


init : String -> String -> String -> Model
init token email phone =
    { token = token
    , firstName = ""
    , lastName = ""
    , email = email
    , phone = phone
    , consent1 = False
    , consent2 = False
    , step = ProfileInfo
    , travelCardState = MaskedInput.initState
    , travelCard = ""
    , travelCardSaved = False
    , validationErrors = []
    }


update : Msg -> Environment -> Model -> PageUpdater Model Msg
update msg env model =
    case msg of
        InputEmail value ->
            PageUpdater.init
                { model
                    | email = value
                    , validationErrors = (V.remove EmailField >> V.remove RegisterForm) model.validationErrors
                }

        InputFirstName value ->
            PageUpdater.init { model | firstName = value, validationErrors = V.remove RegisterForm model.validationErrors }

        InputLastName value ->
            PageUpdater.init { model | lastName = value, validationErrors = V.remove RegisterForm model.validationErrors }

        ToggleConsent1 value ->
            PageUpdater.init { model | consent1 = value }

        ToggleConsent2 value ->
            PageUpdater.init { model | consent2 = value }

        Register ->
            case validateEmail model of
                Ok _ ->
                    PageUpdater.fromPair
                        ( { model | validationErrors = V.init }
                        , saveProfile { env | token = model.token }
                            model.firstName
                            model.lastName
                            model.phone
                            model.email
                        )

                Err errors ->
                    PageUpdater.init { model | validationErrors = errors }

        ReceiveRegisterProfile result ->
            case result of
                Ok () ->
                    PageUpdater.init { model | validationErrors = V.init, travelCardSaved = True }
                        |> PageUpdater.addCmd (Util.Task.doTask NextStep)

                Err error ->
                    let
                        errorMessage =
                            getError error |> Maybe.withDefault "Ukjent feil"
                    in
                        PageUpdater.init { model | validationErrors = V.add [ RegisterForm ] errorMessage model.validationErrors }

        InputTravelCard value ->
            PageUpdater.init { model | travelCard = value }

        StateTravelCard state ->
            PageUpdater.init { model | travelCardState = state, validationErrors = V.remove TravelCardField model.validationErrors }

        RegisterTravelCard ->
            case validateTravelCard model of
                Ok _ ->
                    PageUpdater.fromPair
                        ( { model | validationErrors = V.remove TravelCardField model.validationErrors }
                        , registerTravelCard { env | token = model.token } model.travelCard
                        )

                Err errors ->
                    PageUpdater.init { model | validationErrors = errors }

        ReceiveRegisterTravelCard result ->
            case result of
                Ok () ->
                    PageUpdater.init { model | validationErrors = V.init, travelCardSaved = True }
                        |> PageUpdater.addCmd (Util.Task.doTask NextStep)

                Err error ->
                    PageUpdater.init
                        { model
                            | validationErrors =
                                V.add [ TravelCardField ] (errorToString error) model.validationErrors
                        }

        SkipRegister ->
            PageUpdater.fromPair
                ( { model | validationErrors = V.init, step = Consents }
                , Cmd.none
                  -- skipRegisterProfile { env | token = model.token }
                )

        Finish ->
            PageUpdater.fromPair ( model, MiscService.onboardingDone () )

        NextStep ->
            let
                maybeNextStep =
                    nextStep model.step
            in
                case maybeNextStep of
                    Nothing ->
                        PageUpdater.fromPair ( model, MiscService.onboardingDone () )

                    Just next ->
                        PageUpdater.init { model | step = next, validationErrors = V.init }

        PrevStep ->
            let
                maybePrevStep =
                    prevStep model.step
            in
                case maybePrevStep of
                    Nothing ->
                        PageUpdater.init model

                    Just prev ->
                        PageUpdater.init { model | step = prev, validationErrors = V.init }


getError : Http.Error -> Maybe String
getError error =
    case error of
        Http.BadStatus response ->
            Decode.decodeString
                (Decode.field "upstreamError" Decode.string
                    |> Decode.andThen
                        (\upstreamError ->
                            case
                                Decode.decodeString (Decode.field "shortNorwegian" Decode.string) upstreamError
                            of
                                Err _ ->
                                    Decode.fail "Invalid error"

                                Ok value ->
                                    Decode.succeed value
                        )
                )
                response.body
                |> Result.toMaybe

        _ ->
            Nothing


validateTravelCard : Model -> Result (List (FormError FieldName)) (Valid Model)
validateTravelCard =
    V.validate (V.travelCardValidator TravelCardField .travelCard)


validateEmail : Model -> Result (List (FormError FieldName)) (Valid Model)
validateEmail model =
    if String.isEmpty model.email then
        V.validate V.void model

    else
        V.validate (V.emailValidator EmailField .email) model


view : Environment -> Model -> Html Msg
view env model =
    case model.step of
        ProfileInfo ->
            viewProfileInfo env model
                |> wrapHeader model True "Profilinformasjon (1 av 4)"

        Consents ->
            viewConsents env model
                |> wrapHeader model True "Samtykker (2 av 4)"

        TravelCard ->
            viewTravelCard env model
                |> wrapHeader model False "Legg til t:kort (3 av 4)"

        AppAdvert ->
            viewAppAdvert env model
                |> wrapHeader model True "Har du prøvd AtB-appen?"


wrapHeader : Model -> Bool -> String -> List (Html Msg) -> Html Msg
wrapHeader model narrowPage title children =
    H.div []
        [ PH.init
            |> PH.setTitle (Just title)
            |> PH.setOnCancel (Just ( "Hopp over dette steget", Icon.rightArrow, NextStep ))
            |> PH.setBackButton
                (if model.step == ProfileInfo then
                    Nothing

                 else
                    Just ( "Tilbake", E.onClick PrevStep )
                )
            |> PH.view
        , H.div
            [ A.classList
                [ ( "page", True )
                , ( "page--narrow", narrowPage )
                ]
            ]
            children
        ]


viewProfileInfo : Environment -> Model -> List (Html Msg)
viewProfileInfo _ model =
    [ Section.view
        [ Section.viewPaddedItem
            [ H.p [] [ H.text "Her ber vi om noen opplysninger som vil forenkle bruken av nettbutikken." ]
            ]
        , sectionTextInput "firstname"
            model.firstName
            InputFirstName
            "Fornavn"
            "Skriv inn fornavnet ditt"
        , sectionTextInput "lastname"
            model.lastName
            InputLastName
            "Etternavn"
            "Skriv inn etternavnet ditt"
        , Section.viewItem
            [ TextInput.init "email"
                |> TextInput.setTitle (Just "E-postadresse")
                |> TextInput.setPlaceholder "Hvor skal vi sende kvitteringer?"
                |> TextInput.setOnInput (Just InputEmail)
                |> TextInput.setValue (Just model.email)
                |> TextInput.setError (V.select EmailField model.validationErrors)
                |> TextInput.view
            ]
        , Html.Extra.viewMaybe Message.error <| V.select RegisterForm model.validationErrors
        , Button.init "Neste"
            |> Button.setIcon (Just Icon.rightArrow)
            |> Button.setOnClick (Just Register)
            |> Button.primaryDefault
        ]
    ]


viewConsents : Environment -> Model -> List (Html Msg)
viewConsents _ model =
    [ Section.view
        [ Section.viewPaddedItem
            [ H.p [] [ H.text "Vi trenger komme i kontakt med deg som reisende for å optimalisere opplevelsen av den nye nettbutikken. Vi blir veldig glade om samtykker til dette!" ]
            , H.p [] [ H.a [ A.href "add-this" ] [ H.text "Les vår personvernerklæring" ] ]
            ]
        , Section.viewLabelItem "Samtykker"
            [ Checkbox.init "consent1"
                |> Checkbox.setChecked model.consent1
                |> Checkbox.setOnCheck (Just ToggleConsent1)
                |> Checkbox.setTitle "Jeg ønsker å bli kontaktet når endringer skjer i nettbutikken"
                |> Checkbox.view
            , Checkbox.init "consent2"
                |> Checkbox.setChecked model.consent2
                |> Checkbox.setOnCheck (Just ToggleConsent2)
                |> Checkbox.setTitle "Jeg ønsker å delta på brukertester"
                |> Checkbox.view
            ]
        , if model.consent1 || model.consent2 then
            sectionTextInput "consent-email"
                model.email
                InputEmail
                "E-postadresse"
                "Legg inn din e-postadresse"

          else
            H.text ""
        , Button.init "Lagre samtykker"
            |> Button.setIcon (Just Icon.rightArrow)
            |> Button.setOnClick (Just NextStep)
            |> Button.primaryDefault
        ]
    ]


viewTravelCard : Environment -> Model -> List (Html Msg)
viewTravelCard _ model =
    if model.travelCardSaved then
        [ H.div [ A.class "onboarding__travelCard" ]
            [ Section.view
                [ Message.info "Du har alt lagt til et t:kort."
                , Section.viewPaddedItem
                    [ H.div [ A.class "onboarding__travelCard__input" ]
                        [ H.div [] [] -- used for placeholder for upcommit box to have CSS work for future use.
                        , Section.viewPaddedItem
                            [ Ui.LabelItem.view "t:kortnummer (16-siffer)"
                                [ H.text (Util.TravelCard.format model.travelCard)
                                ]
                            ]
                        ]
                    , H.img
                        [ A.src "/images/travelcard-help-illustration.svg"
                        , A.class "onboarding__travelCard__illustration"
                        , A.alt "t:kort-nummer finner du i øverst til høyre på t:kortet ditt."
                        ]
                        []
                    ]
                ]
            , H.div [] []
            , Section.view
                [ Button.init "Neste"
                    |> Button.setIcon (Just Icon.rightArrow)
                    |> Button.setOnClick (Just NextStep)
                    |> Button.primaryDefault
                ]
            ]
        ]

    else
        [ H.div [ A.class "onboarding__travelCard" ]
            [ Section.view
                [ Section.viewPaddedItem
                    [ H.div [ A.class "onboarding__travelCard__input" ]
                        [ H.div [] [] -- used for placeholder for upcommit box to have CSS work for future use.
                        , MaskedInput.init "travelCard" InputTravelCard StateTravelCard
                            |> MaskedInput.setTitle (Just "t:kortnummer (16-siffer)")
                            |> MaskedInput.setPlaceholder "Skriv inn t:kortnummer"
                            |> MaskedInput.setPattern "#### #### ########"
                            |> MaskedInput.setBordered True
                            |> MaskedInput.setError (V.select TravelCardField model.validationErrors)
                            |> MaskedInput.view model.travelCardState model.travelCard
                        ]
                    , H.img
                        [ A.src "/images/travelcard-help-illustration.svg"
                        , A.class "onboarding__travelCard__illustration"
                        , A.alt "t:kort-nummer finner du i øverst til høyre på t:kortet ditt."
                        ]
                        []
                    ]
                ]
            , Section.view
                [ Button.init "Jeg bruker ikke t:kort"
                    |> Button.setIcon (Just Icon.rightArrow)
                    |> Button.setOnClick (Just NextStep)
                    |> Button.tertiary
                ]
            , Section.view
                [ Button.init "Legg til t:kort"
                    |> Button.setIcon (Just Icon.rightArrow)
                    |> Button.setOnClick (Just RegisterTravelCard)
                    |> Button.primaryDefault
                ]
            ]
        ]


viewAppAdvert : Environment -> Model -> List (Html Msg)
viewAppAdvert _ _ =
    [ H.div []
        [ H.img [ A.class "onboarding__appIllustration", A.src "/images/app.png", A.alt "Eksempelvisning av hvordan app-en ser ut" ] []
        , Section.view
            [ Section.viewPaddedItem
                [ H.p [] [ H.text "Her kan du planlegge reiser, kjøpe enkelt- eller periodebillett og vise billett ved kontroll. Alt samlet i en app. Billetter du kjøper i nettbutikken vil også være tilgjengelig i appen din." ]
                , H.div
                    [ A.class "onboarding__badgeButtons" ]
                    [ H.a
                        [ A.href "https://apps.apple.com/us/app/id1502395251", A.rel "noopener", A.title "Se AtB beta i App Store", A.target "_blank" ]
                        [ H.img [ A.src "/images/badge-ios.svg", A.alt "iOS badge" ] [] ]
                    , H.a
                        [ A.href "https://play.google.com/store/apps/details?id=no.mittatb.store", A.rel "noopener", A.title "Se AtB beta i Google Play Store", A.target "_blank" ]
                        [ H.img [ A.src "/images/badge-android.svg", A.alt "Android badge" ] [] ]
                    ]
                ]
            , Button.init "Fullfør"
                |> Button.setIcon (Just Icon.rightArrow)
                |> Button.setOnClick (Just Finish)
                |> Button.primaryDefault
            ]
        ]
    ]


sectionTextInput : String -> String -> (String -> msg) -> String -> String -> Html msg
sectionTextInput id value action title placeholder =
    Section.viewItem
        [ TextInput.init id
            |> TextInput.setTitle (Just title)
            |> TextInput.setPlaceholder placeholder
            |> TextInput.setOnInput (Just action)
            |> TextInput.setValue (Just value)
            |> TextInput.view
        ]


nextStep : Step -> Maybe Step
nextStep step =
    case step of
        ProfileInfo ->
            Just Consents

        Consents ->
            Just TravelCard

        TravelCard ->
            Just AppAdvert

        AppAdvert ->
            Nothing


prevStep : Step -> Maybe Step
prevStep step =
    case step of
        Consents ->
            Just ProfileInfo

        TravelCard ->
            Just Consents

        AppAdvert ->
            Just TravelCard

        _ ->
            Nothing


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- INTERNAL


saveProfile : Environment -> String -> String -> String -> String -> Cmd Msg
saveProfile env firstName lastName phone email =
    WebshopService.save env firstName lastName (Just phone) (Just email)
        |> Http.toTask
        |> Task.attempt ReceiveRegisterProfile


skipRegisterProfile : Environment -> String -> Cmd Msg
skipRegisterProfile env phone =
    WebshopService.save env "_" "_" (Just phone) Nothing
        |> Http.toTask
        |> Task.attempt ReceiveRegisterProfile


registerTravelCard : Environment -> String -> Cmd Msg
registerTravelCard env travelCardId =
    travelCardId
        |> Util.TravelCard.extractDigits
        |> WebshopService.addTravelCard env
        |> Http.toTask
        |> Task.attempt ReceiveRegisterTravelCard


{-| TODO this should be deduplicated from Account.elm page and reused.
But currently unsure where mappers like this fit in the current layered arcitechture
-}
errorToString : Http.Error -> String
errorToString error =
    case error of
        BadStatus { status, body } ->
            case status.code of
                500 ->
                    "Det skjedde en feil med tjenesten. Prøv igjen senere."

                409 ->
                    "Dette t:kortet eksisterer ikke eller er allerede registrert."

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
