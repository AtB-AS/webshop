module Page.Login exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Dom as Dom
import Environment exposing (Environment)
import Fragment.Icon as Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import PageUpdater exposing (PageUpdater)
import Service.Phone as PhoneService
import Task
import Ui.Button as B
import Ui.Input.Text as T
import Ui.Section
import Util.Event as EventUtil


type Msg
    = InputPhone String
    | InputCode String
    | Login
    | Confirm
    | RequestCode
    | HandleError String
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
                PageUpdater.fromPair
                    ( { model | loading = True }
                    , PhoneService.phoneLogin fullPhone
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


focusBox : Maybe String -> Cmd Msg
focusBox id =
    id
        |> Maybe.map (\i -> Task.attempt (\_ -> NoOp) (Dom.focus i))
        |> Maybe.withDefault Cmd.none


view : Environment -> Model -> Html Msg
view env model =
    H.div [ A.class "page page--login" ]
        [ H.img [ A.src "/static/images/travel-illustration.svg", A.class "pageLogin__illustration" ] []
        , case model.step of
            StepLogin ->
                viewLogin env model

            StepConfirm ->
                viewConfirm env model
        ]


viewLogin : Environment -> Model -> Html Msg
viewLogin env model =
    H.form [ E.onSubmit Login ]
        [ Ui.Section.view
            [ Ui.Section.viewHeader "Logg inn i AtB nettbutikk"
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
            , B.init "Send engangspassord"
                |> B.setIcon (Just Icon.rightArrow)
                |> B.setOnClick (Just Login)
                |> B.setType "submit"
                |> B.primary B.Primary_2
            ]
        , H.node "atb-login-recaptcha" [] []
        ]


viewConfirm : Environment -> Model -> Html Msg
viewConfirm env model =
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
                    |> T.view
                ]
            , B.init "Logg inn"
                |> B.setIcon (Just Icon.rightArrow)
                |> B.setOnClick (Just Confirm)
                |> B.setType "submit"
                |> B.primary B.Primary_2
            ]
        , H.node "atb-login-recaptcha" [] []
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ PhoneService.onRequestCode RequestCode
        , PhoneService.onError HandleError
        ]
