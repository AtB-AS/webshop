module Page.Onboarding exposing (Model, Msg, init, subscriptions, update, view)

import Data.RefData exposing (Consent)
import Data.Webshop exposing (GivenConsent)
import Dict
import Environment exposing (Environment)
import Fragment.Icon as Icon
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Html.Extra
import Http exposing (Error(..))
import Json.Decode as Decode
import PageUpdater exposing (PageUpdater)
import Service.Misc as MiscService
import Service.Webshop as WebshopService
import Set exposing (Set)
import Shared exposing (Shared)
import Task
import Ui.Button as Button
import Ui.Input.Checkbox as Checkbox
import Ui.Input.MaskedText as MaskedInput
import Ui.Input.Text as TextInput
import Ui.LabelItem
import Ui.Message as Message
import Ui.ProgressHeader as PH
import Ui.Section as Section
import Util.PhoneNumber
import Util.Task
import Util.TravelCard
import Util.Validation as V exposing (FormError, ValidationErrors)
import Validate exposing (Valid)


type Msg
    = InputEmail String
    | InputFirstName String
    | InputLastName String
    | InputPhone String
    | OnInputPhoneFocus
    | ToggleConsent Int Bool
    | Register
    | ReceiveRegisterProfile (Result Http.Error ())
    | SkipRegister
    | InputTravelCard String
    | InputStateTravelCard MaskedInput.State
    | RegisterTravelCard
    | ReceiveRegisterTravelCard (Result Http.Error ())
    | InputConsentEmail String
    | RegisterConsents
    | ReceiveRegisterConsent (Result Http.Error GivenConsent)
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
    | PhoneField
    | RegisterForm
    | ConsentForm


type alias Model =
    { token : String
    , firstName : String
    , lastName : String
    , profileSaved : Bool
    , email : String
    , phone : String
    , step : Step
    , travelCard : String
    , travelCardState : MaskedInput.State
    , travelCardSaved : Bool
    , validationErrors : ValidationErrors FieldName
    , consents : Set Int
    , unsavedConsents : List Int
    , consentEmail : String
    , savedEmail : String
    , isReadonlyEmail : Bool
    , isReadonlyPhone : Bool
    }


init : String -> String -> String -> Model
init token email phone =
    { token = token
    , firstName = ""
    , lastName = ""
    , email = email
    , profileSaved = False
    , phone = phone
    , step = ProfileInfo
    , travelCardState = MaskedInput.initState
    , travelCard = ""
    , travelCardSaved = False
    , validationErrors = []
    , consents = Set.empty
    , unsavedConsents = []
    , consentEmail = ""
    , savedEmail = ""
    , isReadonlyEmail = not <| String.isEmpty email
    , isReadonlyPhone = not <| String.isEmpty phone
    }


update : Msg -> Environment -> Shared -> Model -> PageUpdater Model Msg
update msg env shared model =
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

        ToggleConsent id value ->
            let
                newConsents =
                    if value then
                        Set.insert id model.consents

                    else
                        Set.remove id model.consents
            in
                PageUpdater.init { model | consents = newConsents }

        InputPhone value ->
            PageUpdater.init { model | phone = value, validationErrors = (V.remove PhoneField >> V.remove RegisterForm) model.validationErrors }

        OnInputPhoneFocus ->
            PageUpdater.init
                { model
                    | phone =
                        if String.isEmpty model.phone then
                            "+47"

                        else
                            model.phone
                }

        Register ->
            let
                modelWithCountryCode =
                    { model | phone = Util.PhoneNumber.withDefaultCountryCode model.phone }
            in
                case validatePersonalInfo modelWithCountryCode of
                    Ok _ ->
                        PageUpdater.fromPair
                            ( { model | validationErrors = V.init }
                            , saveProfile { env | token = model.token }
                                modelWithCountryCode.firstName
                                modelWithCountryCode.lastName
                                modelWithCountryCode.phone
                                modelWithCountryCode.email
                            )

                    Err errors ->
                        PageUpdater.init { model | validationErrors = errors }

        ReceiveRegisterProfile result ->
            case result of
                Ok () ->
                    PageUpdater.init
                        { model
                            | validationErrors = V.init
                            , profileSaved = True
                            , savedEmail = model.email
                        }
                        |> PageUpdater.addCmd (MiscService.onboardingRefreshAuth ())
                        |> PageUpdater.addCmd (Util.Task.doTask NextStep)

                Err error ->
                    let
                        errorMessage =
                            getError error |> Maybe.withDefault "Ukjent feil"
                    in
                        PageUpdater.init { model | validationErrors = V.add [ RegisterForm ] errorMessage model.validationErrors }

        InputTravelCard value ->
            PageUpdater.init { model | travelCard = value }

        InputStateTravelCard state ->
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
                                V.add [ TravelCardField ] (Util.TravelCard.serverErrorToString WebshopService.travelCardErrorDecoder error) model.validationErrors
                        }

        InputConsentEmail value ->
            PageUpdater.init
                { model
                    | consentEmail = value
                    , validationErrors = (V.remove EmailField >> V.remove ConsentForm) model.validationErrors
                }

        RegisterConsents ->
            let
                consentIds =
                    List.map .id shared.consents

                validUpdater =
                    PageUpdater.fromPair
                        ( { model
                            | unsavedConsents = consentIds
                            , validationErrors = V.init
                          }
                        , registerConsents
                            env
                            consentIds
                            model.consents
                            model.consentEmail
                        )
            in
                if not (Set.isEmpty model.consents) then
                    case validateEmail True .consentEmail model of
                        Ok _ ->
                            validUpdater

                        Err errors ->
                            PageUpdater.init { model | validationErrors = errors }

                else
                    -- No consents were ticked, but we still register all of them as not accepted.
                    validUpdater

        ReceiveRegisterConsent result ->
            case result of
                Ok givenConsent ->
                    let
                        newModel =
                            { model | unsavedConsents = List.filter ((/=) givenConsent.consentId) model.unsavedConsents }
                    in
                        if List.isEmpty newModel.unsavedConsents then
                            PageUpdater.init newModel
                                |> PageUpdater.addCmd (Util.Task.doTask NextStep)

                        else
                            PageUpdater.init newModel

                Err error ->
                    let
                        errorMessage =
                            getError error |> Maybe.withDefault "Ukjent feil"
                    in
                        PageUpdater.init { model | validationErrors = V.add [ ConsentForm ] errorMessage model.validationErrors }

        SkipRegister ->
            PageUpdater.fromPair
                ( { model | validationErrors = V.init }
                , skipRegisterProfile { env | token = model.token } model.phone
                )

        Finish ->
            PageUpdater.fromPair ( model, MiscService.onboardingDone () )

        NextStep ->
            let
                maybeNextStep =
                    nextStep model.step
            in
                case maybeNextStep of
                    -- if next step is travel card and we are not saved, save an empty profile with phone.
                    Just TravelCard ->
                        if model.profileSaved then
                            PageUpdater.init { model | step = TravelCard, validationErrors = V.init }
                                |> (PageUpdater.addGlobalAction <| GA.FocusItem <| Just "travelCard")

                        else
                            PageUpdater.init model
                                |> PageUpdater.addCmd (Util.Task.doTask SkipRegister)

                    Just Consents ->
                        if model.profileSaved then
                            PageUpdater.init
                                { model
                                    | step = Consents
                                    , consentEmail = model.savedEmail
                                    , validationErrors = V.init
                                }
                                |> (PageUpdater.addGlobalAction <| GA.FocusItem <| Just "progress-header")

                        else
                            PageUpdater.init model
                                |> PageUpdater.addCmd (Util.Task.doTask SkipRegister)

                    Nothing ->
                        PageUpdater.fromPair ( model, MiscService.onboardingDone () )

                    Just next ->
                        PageUpdater.init { model | step = next, validationErrors = V.init }
                            |> (PageUpdater.addGlobalAction <| GA.FocusItem <| Just "progress-header")

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
                            |> (PageUpdater.addGlobalAction <| GA.FocusItem <| Just "progress-header")


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


validateEmail : Bool -> (Model -> String) -> Model -> Result (List (FormError FieldName)) (Valid Model)
validateEmail required field model =
    if not required && String.isEmpty (field model) then
        V.validate V.void model

    else
        V.validate (V.emailValidator EmailField field) model


validatorEmail : Model -> Validate.Validator (FormError FieldName) Model
validatorEmail model =
    if String.isEmpty model.email then
        V.void

    else
        V.emailValidator EmailField .email


validatorPhone : Model -> Validate.Validator (FormError FieldName) Model
validatorPhone model =
    if String.isEmpty model.phone then
        V.void

    else
        V.phoneValidator PhoneField .phone


validatePersonalInfo : Model -> Result (List (FormError FieldName)) (Valid Model)
validatePersonalInfo model =
    V.validate (V.all [ validatorEmail model, validatorPhone model ]) model


view : Environment -> Shared -> Model -> Html Msg
view env shared model =
    case model.step of
        ProfileInfo ->
            viewProfileInfo env model
                |> wrapHeader model True "Profilinformasjon"

        Consents ->
            viewConsents env shared model
                |> wrapHeader model True "Samtykker"

        TravelCard ->
            viewTravelCard env model
                |> wrapHeader model False "Legg til t:kort"

        AppAdvert ->
            viewAppAdvert env model
                |> wrapHeader model True "Har du prøvd appen?"


wrapHeader : Model -> Bool -> String -> List (Html Msg) -> Html Msg
wrapHeader model narrowPage title children =
    H.div []
        [ PH.init title
            |> PH.setNext
                (Just
                    ( if model.step == AppAdvert then
                        "Fullfør"

                      else
                        "Hopp over"
                    , NextStep
                    )
                )
            |> PH.setTotalSteps 3
            |> PH.setStep (stepNumber model.step)
            |> PH.setBack
                (if model.step == ProfileInfo then
                    Nothing

                 else
                    Just ( "Tilbake", PrevStep )
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
        , Section.viewItem
            [ TextInput.init "firstname"
                |> TextInput.setTitle (Just "Fornavn")
                |> TextInput.setPlaceholder "Skriv inn fornavnet ditt"
                |> TextInput.setOnInput (Just InputFirstName)
                |> TextInput.setValue (Just model.firstName)
                |> TextInput.view
            ]
        , Section.viewItem
            [ TextInput.init "lastname"
                |> TextInput.setTitle (Just "Etternavn")
                |> TextInput.setPlaceholder "Skriv inn etternavnet ditt"
                |> TextInput.setOnInput (Just InputLastName)
                |> TextInput.setValue (Just model.lastName)
                |> TextInput.view
            ]
        , Section.viewItem
            [ TextInput.init "phone"
                |> TextInput.setTitle
                    (Just <|
                        if model.isReadonlyPhone then
                            "Telefonnummer (fra innlogging)"

                        else
                            "Telefonnummer"
                    )
                |> TextInput.setPlaceholder "Skriv inn ditt telefonnummer"
                |> TextInput.setOnInput (Just InputPhone)
                |> TextInput.setValue (Just model.phone)
                |> TextInput.setError (V.select PhoneField model.validationErrors)
                |> TextInput.setAttributes [ A.readonly model.isReadonlyPhone, E.onFocus OnInputPhoneFocus ]
                |> TextInput.view
            ]
        , Section.viewItem
            [ TextInput.init "email"
                |> TextInput.setTitle
                    (Just <|
                        if model.isReadonlyEmail then
                            "E-postadresse (fra innlogging)"

                        else
                            "E-postadresse"
                    )
                |> TextInput.setPlaceholder "Hvor skal vi sende kvitteringer?"
                |> TextInput.setOnInput (Just InputEmail)
                |> TextInput.setValue (Just model.email)
                |> TextInput.setError (V.select EmailField model.validationErrors)
                |> TextInput.setAttributes [ A.readonly model.isReadonlyEmail ]
                |> TextInput.view
            ]
        , Html.Extra.viewMaybe Message.error <| V.select RegisterForm model.validationErrors
        , Button.init "Neste"
            |> Button.setIcon (Just Icon.rightArrow)
            |> Button.setOnClick (Just Register)
            |> Button.primaryDefault
        ]
    ]


viewConsents : Environment -> Shared -> Model -> List (Html Msg)
viewConsents env shared model =
    [ Section.view
        [ Section.viewPaddedItem
            [ H.p [] [ H.text "For å forbedre nettbutikken og dele relevant informasjon, ber vi om samtykke til å kontakte deg per e-post. Samtykker kan du endre på når som helst!" ]
            , H.p [] [ H.a [ A.href "https://beta.atb.no/private-policy", A.target "_blank" ] [ H.text "Les vår personvernerklæring (åpner nytt vindu)" ] ]
            ]
        , Section.viewLabelItem "Velg samtykker" (List.filterMap (viewConsent model) shared.consents)
        , if not (Set.isEmpty model.consents) && String.isEmpty model.savedEmail then
            Section.viewItem
                [ TextInput.init "consent-email"
                    |> TextInput.setTitle (Just "E-postadresse")
                    |> TextInput.setPlaceholder "Hvor kan vi kontakte deg?"
                    |> TextInput.setOnInput (Just InputConsentEmail)
                    |> TextInput.setValue (Just model.consentEmail)
                    |> TextInput.setError (V.select EmailField model.validationErrors)
                    |> TextInput.view
                ]

          else
            H.text ""
        , Html.Extra.viewMaybe Message.error <| V.select ConsentForm model.validationErrors
        , Button.init "Lagre mine samtykker"
            |> Button.setIcon (Just Icon.rightArrow)
            |> Button.setOnClick (Just RegisterConsents)
            |> Button.setDisabled (env.customerId == Nothing)
            |> Button.primaryDefault
        ]
    ]


viewConsent : Model -> Consent -> Maybe (Html Msg)
viewConsent model consent =
    Dict.get "nob" consent.title
        |> Maybe.andThen
            (\title ->
                Checkbox.init ("consent" ++ String.fromInt consent.id)
                    |> Checkbox.setChecked (Set.member consent.id model.consents)
                    |> Checkbox.setOnCheck (Just <| ToggleConsent consent.id)
                    |> Checkbox.setTitle title
                    |> Checkbox.view
                    |> Just
            )


viewTravelCard : Environment -> Model -> List (Html Msg)
viewTravelCard _ model =
    if model.travelCardSaved then
        [ H.div [ A.class "onboarding__travelCard" ]
            [ Section.view
                [ Message.info "Du har alt lagt til et reisekort."
                , Section.viewPaddedItem
                    [ H.div [ A.class "onboarding__travelCard__input" ]
                        [ Section.viewPaddedItem
                            [ Ui.LabelItem.view "Reisekortnummer (16-siffer)"
                                [ H.text (Util.TravelCard.format model.travelCard)
                                ]
                            ]
                        ]
                    , H.img
                        [ A.src "/org/images/travelcard-help-illustration.svg"
                        , A.class "onboarding__travelCard__illustration"
                        , A.alt "Ditt reisekortnummer finner du på baksiden av reisekortet ditt."
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
                        [ MaskedInput.init "travelCard" InputTravelCard InputStateTravelCard
                            |> MaskedInput.setTitle (Just "t:kortnummer (16-siffer)")
                            |> MaskedInput.setPlaceholder "Skriv inn t:kortnummer"
                            |> MaskedInput.setPattern "#### #### ########"
                            |> MaskedInput.setBordered True
                            |> MaskedInput.setError (V.select TravelCardField model.validationErrors)
                            |> MaskedInput.setAttributes
                                [ A.attribute "inputmode" "numeric"
                                ]
                            |> MaskedInput.view model.travelCardState model.travelCard
                        ]
                    , H.img
                        [ A.src "/org/images/travelcard-help-illustration.svg"
                        , A.class "onboarding__travelCard__illustration"
                        , A.alt "Ditt reisekortnummer finner du på baksiden av reisekortet ditt."
                        ]
                        []
                    ]
                , Section.viewHorizontalGroup
                    [ Button.init "Legg til t:kort"
                        |> Button.setIcon (Just Icon.checkmark)
                        |> Button.setOnClick (Just RegisterTravelCard)
                        |> Button.primaryDefault
                    , Button.init "Legg til senere"
                        |> Button.setIcon (Just Icon.rightArrow)
                        |> Button.setOnClick (Just NextStep)
                        |> Button.tertiary
                    ]
                ]
            ]
        ]


viewAppAdvert : Environment -> Model -> List (Html Msg)
viewAppAdvert _ _ =
    [ H.div []
        [ H.img [ A.class "onboarding__appIllustration", A.src "/images/app.png", A.alt "Eksempelvisning av hvordan app-en ser ut" ] []
        , Section.view
            [ Section.viewPaddedItem
                [ H.p [] [ H.text "Her kan du planlegge reiser, kjøpe billett og vise billett ved kontroll. Alt i en app!" ]
                , H.div
                    [ A.class "onboarding__badgeButtons" ]
                    [ H.a
                        [ A.href "https://apps.apple.com/us/app/id1502395251", A.rel "noopener", A.title "Se AtB-app i App Store (åpner nytt vindu)", A.target "_blank" ]
                        [ H.img [ A.src "/common/images/badge-ios.svg", A.alt "iOS badge" ] [] ]
                    , H.a
                        [ A.href "https://play.google.com/store/apps/details?id=no.mittatb.store", A.rel "noopener", A.title "Se AtB-app i Google Play Store (åpner nytt vindu)", A.target "_blank" ]
                        [ H.img [ A.src "/common/images/badge-android.svg", A.alt "Android badge" ] [] ]
                    ]
                ]
            , Button.init "Fullfør"
                |> Button.setIcon (Just Icon.rightArrow)
                |> Button.setAttributes [ A.title "Aktiver for å avslutte onboarding" ]
                |> Button.setOnClick (Just Finish)
                |> Button.primaryDefault
            ]
        ]
    ]


nextStep : Step -> Maybe Step
nextStep step =
    case step of
        ProfileInfo ->
            Just Consents

        Consents ->
            Just TravelCard

        _ ->
            Nothing



-- TravelCard ->
--     Just AppAdvert
-- AppAdvert ->
--     Nothing


stepNumber : Step -> Int
stepNumber step =
    case step of
        ProfileInfo ->
            1

        Consents ->
            2

        TravelCard ->
            3

        AppAdvert ->
            4


prevStep : Step -> Maybe Step
prevStep step =
    case step of
        Consents ->
            Just ProfileInfo

        TravelCard ->
            Just Consents

        -- AppAdvert ->
        --     Just TravelCard
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
        |> WebshopService.addTravelCard env
        |> Http.toTask
        |> Task.attempt ReceiveRegisterTravelCard


registerConsents : Environment -> List Int -> Set Int -> String -> Cmd Msg
registerConsents env consents choices email =
    consents
        |> List.map
            (\id ->
                WebshopService.registerConsent env id (Set.member id choices) email
                    |> Http.toTask
                    |> Task.attempt ReceiveRegisterConsent
            )
        |> Cmd.batch
