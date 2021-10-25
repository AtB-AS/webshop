module Page.VerifyUser exposing (Model, Msg, init, subscriptions, update, view)

import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Extra
import Notification
import PageUpdater exposing (PageUpdater)
import Service.FirebaseAuth as FirebaseAuth exposing (FirebaseError)
import Ui.Button as B
import Ui.Message
import Ui.Section


type Msg
    = Finish
    | SendVerifyUser
    | CheckVerifyUser
    | VerificationRequested (Maybe FirebaseError)
    | VerificationResponse Bool


type alias Model =
    { email : String
    , error : Maybe String
    }


init : String -> Model
init email =
    { email = email
    , error = Nothing
    }


update : Msg -> Model -> PageUpdater Model Msg
update msg model =
    case msg of
        Finish ->
            PageUpdater.init model

        SendVerifyUser ->
            PageUpdater.fromPair ( { model | error = Nothing }, sendVerifyUser model.email )

        CheckVerifyUser ->
            PageUpdater.fromPair ( { model | error = Nothing }, FirebaseAuth.checkVerifyUser () )

        VerificationRequested _ ->
            PageUpdater.init model
                |> PageUpdater.addGlobalAction
                    ("Sendt bekreftelsesepost. Se e-post for å bekrefte endringen."
                        |> Ui.Message.valid
                        |> (\s -> Notification.setContent s Notification.init)
                        |> GA.ShowNotification
                    )

        VerificationResponse isVerified ->
            PageUpdater.init
                { model
                    | error =
                        if isVerified then
                            Nothing

                        else
                            Just "Ser ikke ut til at du har verifisert e-posten enda. Om du ikke har mottatt e-post kan du sjekke spam-filter eller prøve å sende ny bekreftelse"
                }


view : Model -> Html Msg
view model =
    H.div []
        [ H.div [ A.class "page page--narrow" ]
            [ H.img [ A.src "/org/images/travel-illustration.svg", A.class "pageLogin__illustration", A.alt "", A.attribute "role" "presentation" ] []
            , H.div []
                [ Ui.Section.view
                    [ Ui.Section.viewHeader <| "Bekreft " ++ model.email
                    , Ui.Section.viewPaddedItem [ H.p [] [ H.text "Hei! Du må bekrefte eposten din for å kunne logge inn." ] ]
                    , Html.Extra.viewMaybe Ui.Message.warning model.error
                    , B.init "Det er gjort"
                        |> B.setOnClick (Just CheckVerifyUser)
                        |> B.primary B.Primary_2
                    ]
                , B.init "Send bekreftelse på nytt"
                    |> B.setOnClick (Just SendVerifyUser)
                    |> B.setType "button"
                    |> B.link
                ]
            ]
        ]


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ FirebaseAuth.onVerifyUserRequested VerificationRequested
        , FirebaseAuth.checkVerifyUserResponse VerificationResponse
        ]



-- INTERNAL


sendVerifyUser : String -> Cmd Msg
sendVerifyUser email =
    FirebaseAuth.verifyUser email
