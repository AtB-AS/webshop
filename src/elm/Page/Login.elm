module Page.Login exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser.Dom as Dom
import Browser.Navigation as Nav
import Environment exposing (Environment)
import Fragment.Icon as Icon
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Html.Extra
import Notification
import PageUpdater exposing (PageUpdater)
import Route exposing (LoginMethodPath(..))
import Service.FirebaseAuth as FirebaseAuth exposing (Provider(..))
import Task
import Ui.Button as B
import Ui.Input.Text as T
import Ui.Message as Message
import Ui.PageHeader as PH
import Ui.Section


type LoginMethod
    = PhoneMethod
    | EmailMethod
    | RegisterEmailMethod
    | ResetEmailMethod


type Msg
    = OnEnterPage LoginMethodPath
    | InputPhone String
    | InputCode String
    | InputEmail String
    | InputPassword String
    | DoLogin
    | ResendPhoneCode
    | ResetPassword
    | SetLoginMethod LoginMethod
    | Confirm
    | BackLogin
    | RequestCode
    | HandleError String
    | LoggedIn
    | NoOp


type alias Model =
    { phone : String
    , code : String
    , email : String
    , password : String
    , loginMethod : LoginMethod
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
      , email = ""
      , password = ""
      , loginMethod = PhoneMethod
      , step = StepLogin
      , error = Nothing
      , loading = False
      }
    , Cmd.batch [ focusBox (Just "phone"), focusBox (Just "email") ]
    )


update : Msg -> Environment -> Model -> Nav.Key -> PageUpdater Model Msg
update msg env model navKey =
    case msg of
        OnEnterPage path ->
            let
                isLoggedIn =
                    env.customerId /= Nothing
            in
                PageUpdater.fromPair
                    ( { model | loginMethod = Debug.log " dsata" (methodPathToPath path) }
                      -- Not logged in, so just redirect home
                    , if isLoggedIn then
                        Route.modifyUrl navKey Route.Home

                      else
                        Tuple.second init
                    )

        InputPhone value ->
            PageUpdater.init { model | phone = value }

        InputCode value ->
            PageUpdater.init { model | code = value }

        InputEmail value ->
            PageUpdater.init { model | email = value }

        InputPassword value ->
            PageUpdater.init { model | password = value }

        SetLoginMethod method ->
            PageUpdater.init { model | loginMethod = method }

        BackLogin ->
            PageUpdater.init { model | step = StepLogin, error = Nothing, loading = False, code = "" }

        DoLogin ->
            PageUpdater.fromPair
                ( { model | error = Nothing, loading = True }
                , case model.loginMethod of
                    PhoneMethod ->
                        loginUsingPhone model.phone

                    EmailMethod ->
                        loginUsingEmail model.email model.password

                    ResetEmailMethod ->
                        resetUsingEmail model.email

                    RegisterEmailMethod ->
                        registerUsingEmail model.email model.password
                )

        LoggedIn ->
            PageUpdater.init (Tuple.first init)

        ResendPhoneCode ->
            PageUpdater.fromPair
                ( { model | error = Nothing, loading = True }
                , loginUsingPhone model.phone
                )
                |> (H.text "Sendt ny forespørsel etter engangspassord."
                        |> Message.Valid
                        |> Message.message
                        |> (\s -> Notification.setContent s Notification.init)
                        |> GA.ShowNotification
                        |> PageUpdater.addGlobalAction
                   )

        ResetPassword ->
            PageUpdater.init
                { model | loginMethod = EmailMethod }
                |> (H.text "E-post med informasjon for glemt passord sendt."
                        |> Message.Valid
                        |> Message.message
                        |> (\s -> Notification.setContent s Notification.init)
                        |> GA.ShowNotification
                        |> PageUpdater.addGlobalAction
                   )

        Confirm ->
            PageUpdater.fromPair
                ( { model | loading = True }
                , FirebaseAuth.confirmPhone model.code
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


methodPathToPath : LoginMethodPath -> LoginMethod
methodPathToPath path =
    case path of
        RegisterEmailPath ->
            RegisterEmailMethod

        ForgotPasswordPath ->
            ResetEmailMethod

        EmailPath ->
            EmailMethod

        _ ->
            PhoneMethod


loginUsingPhone : String -> Cmd Msg
loginUsingPhone phoneNumber =
    let
        fullPhone =
            if String.startsWith "+" phoneNumber then
                phoneNumber

            else
                "+47" ++ phoneNumber
    in
        FirebaseAuth.loginPhone fullPhone


loginUsingEmail : String -> String -> Cmd Msg
loginUsingEmail email password =
    FirebaseAuth.loginEmail { email = email, password = password }


resetUsingEmail : String -> Cmd Msg
resetUsingEmail email =
    FirebaseAuth.resetPassword email


registerUsingEmail : String -> String -> Cmd Msg
registerUsingEmail email password =
    FirebaseAuth.registerEmail { email = email, password = password }


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
            [ H.img [ A.src "/images/travel-illustration.svg", A.class "pageLogin__illustration", A.alt "", A.attribute "role" "presentation" ] []
            , case model.step of
                StepLogin ->
                    viewLogin model

                StepConfirm ->
                    viewConfirm env model
            , H.node "atb-login-recaptcha" [] []
            ]
        ]


viewLogin : Model -> Html Msg
viewLogin model =
    let
        viewInputs =
            case model.loginMethod of
                PhoneMethod ->
                    viewPhoneInputs

                ResetEmailMethod ->
                    viewResetInputs

                _ ->
                    viewEmailInputs

        ( submitText, description ) =
            case model.loginMethod of
                PhoneMethod ->
                    ( "Send engangspassord"
                    , [ H.text "Ingen profil enda? Vi oppretter den automatisk for deg når du skriver inn og sender telefonnummeret ditt nedenfor. "
                      , H.a
                            [ Route.href <| Route.Login EmailPath
                            ]
                            [ H.text "Eller du kan logge inn med e-post" ]
                      , H.text "."
                      ]
                    )

                EmailMethod ->
                    ( "Logg inn"
                    , [ H.text "Ingen profil enda? "
                      , H.a
                            [ Route.href <| Route.Login RegisterEmailPath
                            ]
                            [ H.text "Opprett og logg inn med ny profil" ]
                      , H.text ". Eller så kan du "
                      , H.a
                            [ Route.href <| Route.Login PhonePath
                            ]
                            [ H.text "logge inn og opprette automatisk med telefonnummer" ]
                      , H.text "."
                      ]
                    )

                RegisterEmailMethod ->
                    ( "Registrer profil"
                    , [ H.text "Opprett ny profil. "
                      , H.a
                            [ Route.href <| Route.Login EmailPath
                            ]
                            [ H.text "Logg inn med eksisterende konto" ]
                      , H.text " eller "
                      , H.a
                            [ Route.href <| Route.Login PhonePath
                            ]
                            [ H.text "logg inn med telefon" ]
                      , H.text "."
                      ]
                    )

                ResetEmailMethod ->
                    ( "Tilbakestill passord"
                    , [ H.text "Be om å tilbakestille passord på profilen. Eller "
                      , H.a
                            [ Route.href <| Route.Login EmailPath
                            ]
                            [ H.text "gå tilbake" ]
                      , H.text "."
                      ]
                    )
    in
        H.form [ E.onSubmit DoLogin ]
            [ Ui.Section.view
                [ Ui.Section.viewHeader "Velkommen til AtBs nettbutikk"
                , Ui.Section.viewPaddedItem [ H.p [] description ]
                , Ui.Section.viewItem <| viewInputs model
                , H.p []
                    [ H.text "I betaperioden har nettbutikken spesielle begrenseninger og forutsetninger. Gjør deg kjent med disse før du logger inn. "
                    , H.a [ A.href "https://beta.atb.no/onboarding/nettbutikk", A.target "_blank", A.title "Les mer om begrensninger og forutsetninger for piloten på AtBeta (åpner ny side)" ] [ H.text "Begrensninger og forutsetninger (åpner ny side)." ]
                    ]
                    |> Message.Warning
                    |> Message.message
                , B.init submitText
                    |> B.setIcon (Just Icon.rightArrow)
                    |> B.setType "submit"
                    |> B.primary B.Primary_2
                ]
            , B.init "Glemt passord?"
                |> B.setType "button"
                |> B.setElement H.a
                |> B.setAttributes [ Route.href <| Route.Login ForgotPasswordPath ]
                |> B.link
                |> Html.Extra.viewIf (model.loginMethod == EmailMethod)
            ]


viewPhoneInputs : Model -> List (Html Msg)
viewPhoneInputs model =
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


viewEmailInputs : Model -> List (Html Msg)
viewEmailInputs model =
    [ Html.Extra.viewMaybe Message.error model.error
    , T.init "email"
        |> T.setValue (Just model.email)
        |> T.setOnInput (Just InputEmail)
        |> T.setType "email"
        |> T.setRequired True
        |> T.setTitle (Just "E-post")
        |> T.setPlaceholder "Logg inn med e-posten din"
        |> T.view
    , T.init "password"
        |> T.setValue (Just model.password)
        |> T.setOnInput (Just InputPassword)
        |> T.setType "password"
        |> T.setRequired True
        |> T.setTitle (Just "Passord")
        |> T.setPlaceholder "Tast inn passordet ditt"
        |> T.view
    ]


viewResetInputs : Model -> List (Html Msg)
viewResetInputs model =
    [ Html.Extra.viewMaybe Message.error model.error
    , T.init "email"
        |> T.setValue (Just model.email)
        |> T.setOnInput (Just InputEmail)
        |> T.setType "email"
        |> T.setRequired True
        |> T.setTitle (Just "E-post")
        |> T.setPlaceholder "Logg inn med e-posten din"
        |> T.view
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
            |> B.setOnClick (Just ResendPhoneCode)
            |> B.setType "button"
            |> B.link
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ FirebaseAuth.onRequestCode RequestCode
        , FirebaseAuth.onError HandleError
        , FirebaseAuth.onPasswordReset (\_ -> ResetPassword)
        , FirebaseAuth.signedInInfo (\_ -> LoggedIn)
        ]
