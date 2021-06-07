module Page.VerifyUser exposing (Model, Msg, init, subscriptions, update, view)

import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
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


type alias Model =
    String


init : String -> Model
init email =
    email


update : Msg -> Model -> PageUpdater Model Msg
update msg model =
    case msg of
        Finish ->
            PageUpdater.init model

        SendVerifyUser ->
            PageUpdater.fromPair ( model, sendVerifyUser model )

        CheckVerifyUser ->
            PageUpdater.fromPair ( model, FirebaseAuth.checkVerifyUser () )

        VerificationRequested _ ->
            PageUpdater.init model
                |> PageUpdater.addGlobalAction
                    ("Sendt verifikasjonsepost. Se e-post for å bekrefte endringen."
                        |> Ui.Message.valid
                        |> (\s -> Notification.setContent s Notification.init)
                        |> GA.ShowNotification
                    )


view : Model -> Html Msg
view _ =
    H.div []
        [ H.div [ A.class "page page--narrow" ]
            [ H.img [ A.src "/images/travel-illustration.svg", A.class "pageLogin__illustration", A.alt "", A.attribute "role" "presentation" ] []
            , Ui.Section.view
                [ Ui.Section.viewHeader "Verifiser e-post"
                , Ui.Section.viewPaddedItem [ H.p [] [ H.text "Hei! Du må bekrefte eposten din." ] ]
                , B.init "Det er gjort"
                    |> B.setOnClick (Just CheckVerifyUser)
                    |> B.primary B.Primary_2
                ]
            , B.init "Send engangspassord på nytt"
                |> B.setOnClick (Just SendVerifyUser)
                |> B.setType "button"
                |> B.link
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    FirebaseAuth.onVerifyUserRequested VerificationRequested



-- INTERNAL


sendVerifyUser : String -> Cmd Msg
sendVerifyUser email =
    FirebaseAuth.verifyUser email
