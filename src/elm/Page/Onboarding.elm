module Page.Onboarding exposing (Model, Msg, init, subscriptions, update, view)

import Environment exposing (Environment)
import Fragment.Icon as Icon
import GlobalActions as GA
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Events as E
import Http
import Json.Decode as Decode exposing (Decoder)
import Notification
import PageUpdater exposing (PageUpdater)
import Service.Misc as MiscService
import Service.Webshop as WebshopService
import Task
import Ui.Button as Button
import Ui.Input.Checkbox as Checkbox
import Ui.Input.Radio
import Ui.Message as Message
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
                , Cmd.none
                  -- registerTravelCard
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
    H.div [ A.class "page-onboarding" ] <|
        case model.step of
            ProfileInfo ->
                viewProfileInfo env model

            Consents ->
                viewConsents env model

            TravelCard ->
                viewTravelCard env model

            AppAdvert ->
                viewAppAdvert env model


viewProfileInfo : Environment -> Model -> List (Html Msg)
viewProfileInfo env model =
    [ Section.sectionWithOptions { marginBottom = True, marginTop = False }
        [ Section.viewHeader "Profilinformasjon"
        , Section.viewGenericItem [ H.text "Disse opplysningene vil gjøre det enklere for deg å bruke nettbutikken." ]
        , sectionTextInput model.firstName
            InputFirstName
            "Fornavn"
            "Ditt foravn"
        , sectionTextInput model.lastName
            InputLastName
            "Etternavn"
            "Ditt etternavn"
        , sectionTextInput model.email
            InputEmail
            "E-postadresse"
            "Legg inn din e-postadresse"
        , Button.init "Neste"
            |> Button.setIcon (Just Icon.rightArrow)
            |> Button.setOnClick (Just SkipRegister)
            |> Button.primaryDefault
        ]
    ]


viewConsents : Environment -> Model -> List (Html Msg)
viewConsents env model =
    [ Section.sectionWithOptions { marginBottom = True, marginTop = False }
        [ Section.viewHeader "Samtykker"
        , Section.viewGenericItem
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
            sectionTextInput model.email
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
viewTravelCard env model =
    [ H.div [ A.class "onboarding-travelcard" ]
        [ Section.sectionWithOptions { marginBottom = False, marginTop = False }
            [ Section.viewHeader "Legg til t:kort"
            , sectionTextInput model.travelCard
                InputTravelCard
                "t:kort-nummer"
                "Legg til t:kort-nummeret"
            ]
        , Section.sectionWithOptions { marginBottom = False, marginTop = False }
            [ Button.init "Jeg bruker ikke t:kort"
                |> Button.setIcon (Just Icon.rightArrow)
                |> Button.setOnClick (Just SkipTravelCard)
                |> Button.tertiary
            ]
        , Section.sectionWithOptions { marginBottom = False, marginTop = False }
            [ Button.init "Legg til t:kort"
                |> Button.setIcon (Just Icon.rightArrow)
                |> Button.setOnClick (Just RegisterTravelCard)
                |> Button.primaryDefault
            ]
        ]
    ]


viewAppAdvert : Environment -> Model -> List (Html Msg)
viewAppAdvert env model =
    [ Section.sectionWithOptions { marginBottom = False, marginTop = False }
        [ Section.viewHeader "Har du prøvd AtB-appen?"
        , Button.init "Fullfør"
            |> Button.setIcon (Just Icon.rightArrow)
            |> Button.setOnClick (Just Finish)
            |> Button.primaryDefault
        ]
    ]


sectionTextInput : String -> (String -> msg) -> String -> String -> Html msg
sectionTextInput value action title placeholder =
    Section.viewLabelItem title
        [ H.input
            [ A.type_ "text"
            , A.placeholder placeholder
            , E.onInput action
            , A.value value
            ]
            []
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


skipRegisterProfile : Environment -> Cmd Msg
skipRegisterProfile env =
    WebshopService.register env "_" "_" Nothing Nothing
        |> Http.toTask
        |> Task.attempt ReceiveRegisterProfile


registerTravelCard : Environment -> String -> Cmd Msg
registerTravelCard env travelCardId =
    WebshopService.addTravelCard env travelCardId
        |> Http.toTask
        |> Task.attempt ReceiveRegisterTravelCard
