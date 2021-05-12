module Page.Onboarding exposing (Model, Msg, init, subscriptions, update, view)

import Environment exposing (Environment)
import Fragment.Icon as Icon
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
import Http
import Json.Decode as Decode
import Notification
import PageUpdater exposing (PageUpdater)
import Service.Misc as MiscService
import Service.Webshop as WebshopService
import Task
import Ui.Button as Button
import Ui.Input.Checkbox as Checkbox
import Ui.Input.Text as TextInput
import Ui.Message as Message
import Ui.PageHeader as PH
import Ui.Section as Section


type Msg
    = InputEmail String
    | InputFirstName String
    | InputLastName String
    | ToggleConsent1 Bool
    | ToggleConsent2 Bool
    | Register
    | SkipRegister
    | ReceiveRegisterProfile (Result Http.Error ())
    | SkipConsents
    | InputTravelCard String
    | RegisterTravelCard
    | SkipTravelCard
    | ReceiveRegisterTravelCard (Result Http.Error ())
    | Finish
    | CancelOnboarding


type Step
    = ProfileInfo
    | Consents
    | TravelCard
    | AppAdvert


type alias Model =
    { token : String
    , firstName : String
    , lastName : String
    , email : String
    , phone : String
    , consent1 : Bool
    , consent2 : Bool
    , error : Maybe String
    , step : Step
    , travelCard : String
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
    , error = Nothing
    , step = ProfileInfo
    , travelCard = ""
    }


update : Msg -> Environment -> Model -> PageUpdater Model Msg
update msg env model =
    case msg of
        InputEmail value ->
            PageUpdater.init { model | email = value }

        InputFirstName value ->
            PageUpdater.init { model | firstName = value }

        InputLastName value ->
            PageUpdater.init { model | lastName = value }

        ToggleConsent1 value ->
            PageUpdater.init { model | consent1 = value }

        ToggleConsent2 value ->
            PageUpdater.init { model | consent2 = value }

        Register ->
            PageUpdater.fromPair
                ( model
                , registerProfile { env | token = model.token }
                    model.firstName
                    model.lastName
                    model.phone
                    model.email
                )

        SkipRegister ->
            PageUpdater.fromPair
                ( { model | error = Nothing, step = Consents }
                , Cmd.none
                  -- skipRegisterProfile { env | token = model.token }
                )

        ReceiveRegisterProfile result ->
            case result of
                Ok () ->
                    PageUpdater.init { model | error = Nothing, step = TravelCard }

                Err error ->
                    PageUpdater.init model
                        |> PageUpdater.addGlobalAction
                            (error
                                |> getError
                                |> Maybe.withDefault "Det oppstod en feil, prøv på nytt."
                                |> Message.Error
                                |> Message.message
                                |> (\s ->
                                        Notification.setContent s Notification.init
                                   )
                                |> GA.ShowNotification
                            )

        SkipConsents ->
            PageUpdater.init { model | error = Nothing, step = TravelCard }

        InputTravelCard value ->
            PageUpdater.init { model | travelCard = value }

        RegisterTravelCard ->
            PageUpdater.fromPair
                ( model
                , skipRegisterProfile { env | token = model.token } model.phone
                )

        ReceiveRegisterTravelCard result ->
            case result of
                Ok () ->
                    PageUpdater.init { model | error = Nothing, step = TravelCard }

                Err error ->
                    PageUpdater.init model
                        |> PageUpdater.addGlobalAction
                            (error
                                |> getError
                                |> Maybe.withDefault "Det oppstod en feil, prøv på nytt."
                                |> Message.Error
                                |> Message.message
                                |> (\s ->
                                        Notification.setContent s Notification.init
                                   )
                                |> GA.ShowNotification
                            )

        SkipTravelCard ->
            PageUpdater.init { model | error = Nothing, step = AppAdvert }

        Finish ->
            PageUpdater.fromPair ( model, MiscService.onboardingDone () )

        CancelOnboarding ->
            PageUpdater.fromPair ( model, MiscService.onboardingDone () )


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


view : Environment -> Model -> Html Msg
view env model =
    case model.step of
        ProfileInfo ->
            viewProfileInfo env model
                |> wrapHeader True "Profilinformasjon (1 av 4)"

        Consents ->
            viewConsents env model
                |> wrapHeader True "Samtykker (2 av 4)"

        TravelCard ->
            viewTravelCard env model
                |> wrapHeader False "Legg til t:kort (3 av 4)"

        AppAdvert ->
            viewAppAdvert env model
                |> wrapHeader True "Har du prøvd AtB-appen?"


wrapHeader : Bool -> String -> List (Html Msg) -> Html Msg
wrapHeader narrowPage title children =
    H.div []
        [ PH.init
            |> PH.setTitle (Just title)
            |> PH.setOnCancel (Just CancelOnboarding)
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
        , sectionTextInput "email"
            model.email
            InputEmail
            "E-postadresse"
            "Hvor skal vi sende kvitteringer?"
        , Button.init "Neste"
            |> Button.setIcon (Just Icon.rightArrow)
            |> Button.setOnClick (Just SkipRegister)
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
            |> Button.setOnClick (Just SkipConsents)
            |> Button.primaryDefault
        ]
    ]


viewTravelCard : Environment -> Model -> List (Html Msg)
viewTravelCard _ model =
    [ H.div [ A.class "onboarding__travelCard" ]
        [ Section.view
            [ Section.viewItem
                [ H.div [ A.class "onboarding__travelCard__input" ]
                    [ H.div [] [] -- used for placeholder for upcommit box to have CSS work for future use.
                    , TextInput.init "travelCard"
                        |> TextInput.setTitle (Just "t:kort-nummer")
                        |> TextInput.setPlaceholder "Legg til t:kort-nummeret"
                        |> TextInput.setOnInput (Just InputTravelCard)
                        |> TextInput.setValue (Just model.travelCard)
                        |> TextInput.setBordered True
                        |> TextInput.view
                    ]
                ]
            , H.img
                [ A.src "/images/travelcard-help-illustration.svg"
                , A.class "onboarding__travelCard__illustration"
                , A.alt "t:kort-nummer finner du i øverst til høyre på t:kortet ditt."
                ]
                []
            ]
        , Section.view
            [ Button.init "Jeg bruker ikke t:kort"
                |> Button.setIcon (Just Icon.rightArrow)
                |> Button.setOnClick (Just SkipTravelCard)
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
    [ Section.view
        [ Section.viewPaddedItem [ H.text "reklame kommer her" ]
        , Button.init "Fullfør"
            |> Button.setIcon (Just Icon.rightArrow)
            |> Button.setOnClick (Just Finish)
            |> Button.primaryDefault
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- INTERNAL


registerProfile : Environment -> String -> String -> String -> String -> Cmd Msg
registerProfile env firstName lastName phone email =
    WebshopService.register env firstName lastName (Just phone) (Just email)
        |> Http.toTask
        |> Task.attempt ReceiveRegisterProfile


skipRegisterProfile : Environment -> String -> Cmd Msg
skipRegisterProfile env phone =
    WebshopService.register env "_" "_" (Just phone) Nothing
        |> Http.toTask
        |> Task.attempt ReceiveRegisterProfile


registerTravelCard : Environment -> String -> Cmd Msg
registerTravelCard env travelCardId =
    WebshopService.addTravelCard env travelCardId
        |> Http.toTask
        |> Task.attempt ReceiveRegisterTravelCard
