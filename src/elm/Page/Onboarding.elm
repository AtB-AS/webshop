module Page.Onboarding exposing (Model, Msg, init, subscriptions, update, view)

import Environment exposing (Environment)
import Fragment.Icon as Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Http
import Json.Decode as Decode exposing (Decoder)
import PageUpdater exposing (PageUpdater)
import Service.Misc as MiscService
import Service.Webshop as WebshopService
import Task


type Msg
    = InputEmail String
    | InputFirstName String
    | InputLastName String
    | ToggleConsent1 Bool
    | ToggleConsent2 Bool
    | Register
    | SkipRegister
    | ReceiveRegister (Result Http.Error ())


type alias Model =
    { token : String
    , firstName : String
    , lastName : String
    , email : String
    , phone : String
    , consent1 : Bool
    , consent2 : Bool
    , error : Maybe String
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
                , register { env | token = model.token }
                    model.firstName
                    model.lastName
                    model.phone
                    model.email
                )

        SkipRegister ->
            PageUpdater.fromPair
                ( model
                , skipRegister { env | token = model.token } model.phone
                )

        ReceiveRegister result ->
            case result of
                Ok () ->
                    PageUpdater.fromPair ( { model | error = Nothing }, MiscService.onboardingDone () )

                Err error ->
                    PageUpdater.init
                        { model
                            | error =
                                getError error
                                    |> Maybe.withDefault "Det oppstod en feil, prøv på nytt."
                                    |> Just
                        }


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
    H.div [ A.class "page-onboarding" ]
        [ H.div [ A.class "section-box", A.style "width" "320px" ]
            [ H.div []
                [ H.div [ A.style "font-weight" "500", A.style "margin-bottom" "10px" ] [ H.text "Samtykker" ]
                , H.div [] [ H.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec imperdiet ante sit amet purus vulputate luctus. Donec nec urna ut nisl tincidunt congue in in ex. " ]
                ]
            , textInput model.firstName
                InputFirstName
                "Fornavn"
                "Ditt foravn"
            , textInput model.lastName
                InputLastName
                "Etternavn"
                "Ditt etternavn"
            , textInput model.email
                InputEmail
                "E-postadresse"
                "Legg inn din e-postadresse"
            , consent "consent1"
                model.consent1
                ToggleConsent1
                "Jeg ønsker å bli kontaktet når endringer skjer i nettbutikken"
            , consent "consent2"
                model.consent2
                ToggleConsent2
                "Jeg ønsker å delta på brukertester"
            ]
        , button True Register "Lagre samtykker"
        , button False SkipRegister "Hopp over"
        , case model.error of
            Just error ->
                H.div [] [ H.text error ]

            Nothing ->
                H.text ""
        ]


button : Bool -> msg -> String -> Html msg
button primary action title =
    H.div
        [ A.classList
            [ ( "button", primary )
            , ( "no-button", not primary )
            ]
        , E.onClick action
        ]
        [ H.div [] [ H.text title ]
        , Icon.rightArrow
        ]


textInput : String -> (String -> msg) -> String -> String -> Html msg
textInput value action title placeholder =
    H.div []
        [ H.div
            [ A.style "font-weight" "400"
            , A.style "font-size" "12px"
            , A.style "margin-bottom" "10px"
            ]
            [ H.text title ]
        , H.div []
            [ H.input
                [ A.type_ "text"
                , A.placeholder placeholder
                , E.onInput action
                , A.value value
                ]
                []
            ]
        ]


consent : String -> Bool -> (Bool -> msg) -> String -> Html msg
consent id value action title =
    H.div [ A.class "consent" ]
        [ H.div [] [ H.text title ]
        , toggle id value action
        ]


toggle : String -> Bool -> (Bool -> msg) -> Html msg
toggle id value action =
    H.div []
        [ H.input [ A.id id, A.type_ "checkbox", A.checked value, E.onCheck action ] []
        , H.label [ A.for id, A.class "toggle" ]
            [ case value of
                True ->
                    Icon.toggleOn

                False ->
                    Icon.toggleOff
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- INTERNAL


register : Environment -> String -> String -> String -> String -> Cmd Msg
register env firstName lastName phone email =
    WebshopService.register env firstName lastName (Just phone) (Just email)
        |> Http.toTask
        |> Task.attempt ReceiveRegister


skipRegister : Environment -> String -> Cmd Msg
skipRegister env phone =
    WebshopService.register env "_" "_" (Just phone) Nothing
        |> Http.toTask
        |> Task.attempt ReceiveRegister



-- INTERNAL COMPONENTS


actionButton : msg -> String -> Html msg
actionButton action title =
    H.div [] [ H.button [ A.class "action-button", E.onClick action ] [ H.text title ] ]


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
