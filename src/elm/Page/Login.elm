module Page.Login exposing (Model, Msg, init, subscriptions, update, view)

import Environment exposing (Environment)
import Fragment.Icon as Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import PageUpdater exposing (PageUpdater)
import Service.Phone as PhoneService


type Msg
    = InputPhone String
    | InputCode String
    | Login
    | Confirm
    | RequestCode
    | HandleError String


type alias Model =
    { phone : String
    , code : String
    , step : Step
    , error : Maybe String
    }


type Step
    = StepLogin
    | StepConfirm


init : Model
init =
    { phone = ""
    , code = ""
    , step = StepLogin
    , error = Nothing
    }


update : Msg -> Environment -> Model -> PageUpdater Model Msg
update msg env model =
    case msg of
        InputPhone value ->
            PageUpdater.init { model | phone = value }

        InputCode value ->
            PageUpdater.init { model | code = value }

        Login ->
            let
                fullPhone =
                    if String.startsWith "+" model.phone then
                        model.phone

                    else
                        "+47" ++ model.phone
            in
                PageUpdater.fromPair ( model, PhoneService.phoneLogin fullPhone )

        Confirm ->
            PageUpdater.fromPair ( model, PhoneService.phoneConfirm model.code )

        RequestCode ->
            PageUpdater.init { model | step = StepConfirm, code = "", error = Nothing }

        HandleError error ->
            PageUpdater.init { model | error = Just error }


view : Environment -> Model -> Html Msg
view env model =
    case model.step of
        StepLogin ->
            viewLogin env model

        StepConfirm ->
            viewConfirm env model


viewLogin : Environment -> Model -> Html Msg
viewLogin env model =
    H.div [ A.class "login" ]
        [ Icon.wrapper 80 Icon.atb
        , H.div [ A.class "section-box", A.style "width" "320px" ]
            [ H.div [ A.style "font-weight" "500", A.style "margin-bottom" "10px" ]
                [ H.text "Logg inn i AtB nettbutikk" ]
            , textInput model.phone
                InputPhone
                "Telefonnummer"
                "Logg inn med telefonnummeret ditt"
            ]
        , H.node "atb-login-recaptcha" [] []
        , button True Login "Logg inn"
        , case model.error of
            Just error ->
                H.div [] [ H.text error ]

            Nothing ->
                H.text ""
        ]


viewConfirm : Environment -> Model -> Html Msg
viewConfirm env model =
    H.div [ A.class "login" ]
        [ H.div [ A.class "section-box", A.style "width" "320px" ]
            [ H.div [ A.style "font-weight" "500", A.style "margin-bottom" "10px" ]
                [ H.text <| "Vi har sendt et engangspassord til " ++ model.phone ++ ", vennligst skriv det inn nedenfor." ]
            , textInput model.code
                InputCode
                "Engangspassord"
                "Skriv inn engangspassordet"
            ]
        , H.node "atb-login-recaptcha" [] []
        , button True Confirm "Logg inn"
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
    Sub.batch
        [ PhoneService.onRequestCode RequestCode
        , PhoneService.onError HandleError
        ]



-- INTERNAL
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
