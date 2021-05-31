module Page.Login exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Dom as Dom
import Environment exposing (Environment)
import Fragment.Icon as Icon
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Html.Extra
import Notification
import PageUpdater exposing (PageUpdater)
import Service.FirebaseAuth as FirebaseAuth
import Service.Phone as PhoneService
import Task
import Ui.Button as B
import Ui.Input.Text as T
import Ui.Message as Message
import Ui.PageHeader as PH
import Ui.Section


type Msg
    = InputPhone String
    | InputCode String
    | Login
    | Resend
    | Confirm
    | BackLogin
    | RequestCode
    | HandleError String
    | LoggedIn
    | NoOp


type alias Model =
    { phone : String
    , code : String
    , step : Step
    , error : Maybe String
    , loading : Bool
    }


type Step
    = StepLogin
    | StepConfirm


init : ( Model, Cmd Msg )
init =
    ( { phone = ""
      , code = ""
      , step = StepLogin
      , error = Nothing
      , loading = False
      }
    , focusBox (Just "phone")
    )


update : Msg -> Environment -> Model -> PageUpdater Model Msg
update msg _ model =
    case msg of
        InputPhone value ->
            PageUpdater.init { model | phone = value }

        InputCode value ->
            PageUpdater.init { model | code = value }

        BackLogin ->
            PageUpdater.init { model | step = StepLogin, error = Nothing, loading = False, code = "" }

        Login ->
            updateLogin model

        LoggedIn ->
            PageUpdater.init (Tuple.first init)

        Resend ->
            updateLogin model
                |> (H.text "Sendt ny forespørsel etter engangspassord."
                        |> Message.Valid
                        |> Message.message
                        |> (\s -> Notification.setContent s Notification.init)
                        |> GA.ShowNotification
                        |> PageUpdater.addGlobalAction
                   )

        Confirm ->
            PageUpdater.fromPair
                ( { model | loading = True }
                , PhoneService.phoneConfirm model.code
                )

        RequestCode ->
            PageUpdater.fromPair
                ( { model
                    | step = StepConfirm
                    , code = ""
                    , error = Nothing
                    , loading = False
                  }
                , focusBox (Just "confirmbox")
                )

        HandleError error ->
            PageUpdater.init { model | error = Just error, loading = False }

        NoOp ->
            PageUpdater.init model


updateLogin : Model -> PageUpdater Model Msg
updateLogin model =
    let
        fullPhone =
            if String.startsWith "+" model.phone then
                model.phone

            else
                "+47" ++ model.phone
    in
        PageUpdater.fromPair
            ( { model | loading = True }
            , PhoneService.phoneLogin fullPhone
            )


focusBox : Maybe String -> Cmd Msg
focusBox id =
    id
        |> Maybe.map (\i -> Task.attempt (\_ -> NoOp) (Dom.focus i))
        |> Maybe.withDefault Cmd.none


view : Environment -> Model -> Html Msg
view env model =
    H.div []
        [ case model.step of
            StepLogin ->
                Html.Extra.nothing

            StepConfirm ->
                PH.init
                    |> PH.setBackButton (Just ( "Avbryt", E.onClick BackLogin ))
                    |> PH.view
                    |> List.singleton
                    |> H.div [ A.class "pageLogin__header" ]
        , H.div [ A.class "page page--narrow" ]
            [ H.img [ A.src "/images/travel-illustration.svg", A.class "pageLogin__illustration" ] []
            , case model.step of
                StepLogin ->
                    viewLogin env model

                StepConfirm ->
                    viewConfirm env model
            , H.node "atb-login-recaptcha" [] []
            ]
        ]


viewLogin : Environment -> Model -> Html Msg
viewLogin _ model =
    H.form [ E.onSubmit Login ]
        [ Ui.Section.view
            [ Ui.Section.viewHeader "Velkommen til AtBs nettbutikk"
            , Ui.Section.viewPaddedItem [ H.p [] [ H.text "Ingen profil enda? Vi oppretter den automatisk for deg når du skriver inn og sender telefonnummeret ditt nedenfor." ] ]
            , Ui.Section.viewItem
                [ T.init "phone"
                    |> T.setValue (Just model.phone)
                    |> T.setOnInput (Just InputPhone)
                    |> T.setError model.error
                    |> T.setType "tel"
                    |> T.setRequired True
                    |> T.setTitle (Just "Telefonnummer")
                    |> T.setPlaceholder "Logg inn med telefonnummeret ditt"
                    |> T.view
                ]
            , H.p []
                [ H.text "I betaperioden har nettbutikken spesielle begrenseninger og forutsetninger. Gjør deg kjent med disse før du logger inn. "
                , H.a [ A.href "https://beta.atb.no/onboarding/nettbutikk", A.target "_blank", A.title "Les mer om begrensninger og forutsetninger for piloten på AtBeta" ] [ H.text "Begrensninger og forutsetninger (åpner ny side)." ]
                ]
                |> Message.Warning
                |> Message.message
            , B.init "Send engangspassord"
                |> B.setIcon (Just Icon.rightArrow)
                |> B.setType "submit"
                |> B.primary B.Primary_2
            ]
        ]


viewConfirm : Environment -> Model -> Html Msg
viewConfirm _ model =
    H.form [ E.onSubmit Confirm ]
        [ Ui.Section.view
            [ Ui.Section.viewPaddedItem [ H.p [] [ H.text ("Vi har sendt et engangspassord til " ++ model.phone) ] ]
            , Ui.Section.viewItem
                [ T.init "confirmbox"
                    |> T.setValue (Just model.code)
                    |> T.setOnInput (Just InputCode)
                    |> T.setError model.error
                    |> T.setTitle (Just "Engangspassord")
                    |> T.setPlaceholder "Skriv inn engangspassordet"
                    |> T.setAttributes
                        [ A.attribute "autocomplete" "one-time-code"
                        , A.pattern "[0-9]*"
                        , A.attribute "inputmode" "numeric"
                        ]
                    |> T.view
                ]
            , B.init "Logg inn"
                |> B.setIcon (Just Icon.rightArrow)
                |> B.setType "submit"
                |> B.primary B.Primary_2
            ]
        , B.init "Send engangspassord på nytt"
            |> B.setOnClick (Just Resend)
            |> B.setType "button"
            |> B.link
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ PhoneService.onRequestCode RequestCode
        , PhoneService.onError HandleError
        , FirebaseAuth.signInInfo (\_ -> LoggedIn)
        ]
