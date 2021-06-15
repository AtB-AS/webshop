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
import Service.Misc as Misc
import Task
import Ui.Button as B
import Ui.Input.Text as T
import Ui.Message as Message
import Ui.PageHeader as PH
import Ui.Section
import Util.PhoneNumber
import Util.Validation as V exposing (FormError, ValidationErrors)
import Validate exposing (Valid)


type LoginMethod
    = PhoneMethod
    | EmailMethod
    | RegisterEmailMethod
    | ResetEmailMethod


type FieldName
    = EmailField
    | PhoneField
    | PasswordField


type Msg
    = OnEnterPage LoginMethodPath
    | OnLeavePage LoginMethodPath
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
    , validationErrors : ValidationErrors FieldName
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
      , validationErrors = V.init
      }
    , Cmd.batch [ focusBox (Just "phone"), focusBox (Just "email") ]
    )


update : Msg -> Environment -> Model -> Nav.Key -> PageUpdater Model Msg
update msg env model navKey =
    case msg of
        OnEnterPage path ->
            let
                shouldNavigateHome =
                    env.customerId /= Nothing
            in
                PageUpdater.fromPair
                    ( { model | loginMethod = methodPathToPath path, error = Nothing, validationErrors = V.init, loading = False }
                      -- Not logged in, so just redirect home
                    , if shouldNavigateHome then
                        Route.modifyUrl navKey Route.Home

                      else
                        Tuple.second init
                    )

        OnLeavePage _ ->
            PageUpdater.init <| Tuple.first init

        InputPhone value ->
            PageUpdater.init { model | phone = value, validationErrors = V.remove PhoneField model.validationErrors }

        InputCode value ->
            PageUpdater.init { model | code = value }

        InputEmail value ->
            PageUpdater.init { model | email = value, validationErrors = V.remove EmailField model.validationErrors }

        InputPassword value ->
            PageUpdater.init { model | password = value, validationErrors = V.remove PasswordField model.validationErrors }

        SetLoginMethod method ->
            PageUpdater.init { model | loginMethod = method, error = Nothing, validationErrors = V.init, loading = False }

        BackLogin ->
            PageUpdater.init { model | step = StepLogin, error = Nothing, validationErrors = V.init, loading = False, code = "" }

        DoLogin ->
            case validateLogin model of
                Ok _ ->
                    PageUpdater.fromPair
                        ( { model | error = Nothing, validationErrors = V.init, loading = True }
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

                Err errors ->
                    PageUpdater.init { model | validationErrors = errors }

        LoggedIn ->
            PageUpdater.fromPair ( Tuple.first init, Route.newUrl navKey Route.Home )

        ResendPhoneCode ->
            PageUpdater.fromPair
                ( { model | error = Nothing, loading = True }
                , loginUsingPhone model.phone
                )
                |> (H.text "Sendt ny forespørsel etter engangskode."
                        |> Message.Valid
                        |> Message.message
                        |> (\s -> Notification.setContent s Notification.init)
                        |> GA.ShowNotification
                        |> PageUpdater.addGlobalAction
                   )

        ResetPassword ->
            PageUpdater.init
                { model | loginMethod = EmailMethod }
                |> (H.text "E-post med informasjon for endring av passord sendt."
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


validateLogin : Model -> Result (List (FormError FieldName)) (Valid Model)
validateLogin model =
    case model.loginMethod of
        PhoneMethod ->
            validatePhone model

        ResetEmailMethod ->
            validateEmail model

        _ ->
            validateEmailPassword model


validateEmailPassword : Model -> Result (List (FormError FieldName)) (Valid Model)
validateEmailPassword model =
    V.validate (V.all [ V.emailValidator EmailField .email, V.passwordValidator PasswordField .password ]) model


validateEmail : Model -> Result (List (FormError FieldName)) (Valid Model)
validateEmail model =
    V.validate (V.emailValidator EmailField .email) model


validatePhone : Model -> Result (List (FormError FieldName)) (Valid Model)
validatePhone model =
    V.validate (V.phoneValidator PhoneField .phone) model


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
    FirebaseAuth.loginPhone <| Util.PhoneNumber.withCountryCode phoneNumber


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
    H.form [ E.onSubmit DoLogin, A.novalidate True ] <|
        case model.loginMethod of
            PhoneMethod ->
                viewPhoneLogin model

            ResetEmailMethod ->
                viewEmailReset model

            RegisterEmailMethod ->
                viewEmailRegister model

            EmailMethod ->
                viewEmailLogin model


viewPhoneLogin : Model -> List (Html Msg)
viewPhoneLogin model =
    [ Ui.Section.view
        [ viewWelcomeIllustration
        , Ui.Section.viewPaddedItem
            [ H.div [ A.attribute "aria-label" "Logg inn eller opprett en ny AtB-profil med engangskode på telefonen din. Brukere av skjermleser anbefales innlogging med e-post." ]
                [ H.p [ A.attribute "aria-hidden" "true" ] [ H.text "Logg inn eller opprett en ny AtB-profil med engangskode på telefonen din." ]
                ]
            ]
        , Ui.Section.viewItem <| viewPhoneInputs model
        , Ui.Section.viewPaddedItem
            [ H.p [] [ H.a [ Route.href <| Route.Login EmailPath ] [ H.text "Jeg vil heller bruke e-post" ] ]
            ]
        , betaNotice
        , B.init "Send engangskode"
            |> B.setIcon (Just Icon.rightArrow)
            |> B.setType "submit"
            |> B.primary B.Primary_2
        ]
    ]


viewEmailLogin : Model -> List (Html Msg)
viewEmailLogin model =
    [ Ui.Section.view
        [ viewWelcomeIllustration
        , Ui.Section.viewPaddedItem
            [ H.p []
                [ H.text "Logg inn på din AtB-profil med e-post og passord eller "
                , H.a [ Route.href <| Route.Login PhonePath, A.title "Logg på med telefon og engangskode" ] [ H.text "bruk engangskode fra telefonen" ]
                , H.text "."
                ]
            ]
        , Ui.Section.viewItem <| viewEmailInputs "Skriv inn passordet ditt" model
        , Ui.Section.viewPaddedItem
            [ H.p [] [ H.a [ Route.href <| Route.Login RegisterEmailPath ] [ H.text "Opprett en ny profil" ] ]
            ]
        , betaNotice
        , B.init "Logg inn"
            |> B.setIcon (Just Icon.rightArrow)
            |> B.setType "submit"
            |> B.primary B.Primary_2
        ]
    , B.init "Glemt passord?"
        |> B.setElement H.a
        |> B.setAttributes [ Route.href <| Route.Login ForgotPasswordPath ]
        |> B.link
    ]


viewEmailRegister : Model -> List (Html Msg)
viewEmailRegister model =
    [ Ui.Section.view
        [ viewWelcomeIllustration
        , Ui.Section.viewPaddedItem [ H.p [] [ H.text "Opprett ny profil." ] ]
        , Ui.Section.viewItem <| viewEmailInputs "Velg et passord" model
        , betaNotice
        , B.init "Registrer profil"
            |> B.setIcon (Just Icon.rightArrow)
            |> B.setType "submit"
            |> B.primary B.Primary_2
        ]
    , B.init "Logg inn med en eksisterende profil"
        |> B.setElement H.a
        |> B.setAttributes [ Route.href <| Route.Login EmailPath ]
        |> B.link
    ]


viewEmailReset : Model -> List (Html Msg)
viewEmailReset model =
    [ Ui.Section.view
        [ viewWelcomeIllustration
        , Ui.Section.viewPaddedItem [ H.p [] [ H.text "Be om å tilbakestille passord på profilen." ] ]
        , Ui.Section.viewItem <| viewResetInputs model
        , betaNotice
        , B.init "Tilbakestill passord"
            |> B.setIcon (Just Icon.rightArrow)
            |> B.setType "submit"
            |> B.primary B.Primary_2
        ]
    , B.init "Logg inn på min profil"
        |> B.setElement H.a
        |> B.setAttributes [ Route.href <| Route.Login EmailPath ]
        |> B.link
    ]


viewWelcomeIllustration : Html Msg
viewWelcomeIllustration =
    Ui.Section.viewHeaderEl
        [ H.img [ A.src "/images/waving-hand.png", A.alt "", A.attribute "role" "presentation" ] []
        , H.text "Velkommen til AtBs nettbutikk"
        ]


betaNotice : Html msg
betaNotice =
    H.p []
        [ H.text "I BETA har nettbutikken enkelte forutsetninger. Gjør deg kjent med disse før du logger inn. "
        , H.a [ A.href "https://beta.atb.no/onboarding/nettbutikk", A.target "_blank", A.title "Les mer om begrensninger og forutsetninger for piloten på AtBeta (åpner ny side)" ] [ H.text "Forutsetninger (åpner ny side)." ]
        ]
        |> Message.Warning
        |> Message.message


viewPhoneInputs : Model -> List (Html Msg)
viewPhoneInputs model =
    [ T.init "phone"
        |> T.setValue (Just model.phone)
        |> T.setOnInput (Just InputPhone)
        |> T.setError model.error
        |> T.setType "tel"
        |> T.setRequired True
        |> T.setError (V.select PhoneField model.validationErrors)
        |> T.setTitle (Just "Telefonnummer")
        |> T.setPlaceholder "Logg inn med telefonnummeret ditt"
        |> T.view
    ]


viewEmailInputs : String -> Model -> List (Html Msg)
viewEmailInputs passwordPlaceholder model =
    [ Html.Extra.viewMaybe Message.error model.error
    , T.init "email"
        |> T.setValue (Just model.email)
        |> T.setOnInput (Just InputEmail)
        |> T.setType "email"
        |> T.setRequired True
        |> T.setTitle (Just "E-post")
        |> T.setPlaceholder "Skriv inn din e-postadresse"
        |> T.setError (V.select EmailField model.validationErrors)
        |> T.setAttributes
            [ A.attribute "autocomplete" "email"
            ]
        |> T.view
    , T.init "password"
        |> T.setValue (Just model.password)
        |> T.setOnInput (Just InputPassword)
        |> T.setType "password"
        |> T.setRequired True
        |> T.setTitle (Just "Passord")
        |> T.setError (V.select PasswordField model.validationErrors)
        |> T.setAttributes
            [ A.attribute "autocomplete" "password"
            ]
        |> T.setPlaceholder passwordPlaceholder
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
        |> T.setAttributes
            [ A.attribute "autocomplete" "email"
            ]
        |> T.setError (V.select EmailField model.validationErrors)
        |> T.setTitle (Just "E-post")
        |> T.setPlaceholder "Logg inn med e-posten din"
        |> T.view
    ]


viewConfirm : Environment -> Model -> Html Msg
viewConfirm _ model =
    H.form [ E.onSubmit Confirm ]
        [ Ui.Section.view
            [ Ui.Section.viewPaddedItem [ H.p [] [ H.text ("Vi har sendt et engangskode til " ++ model.phone) ] ]
            , Ui.Section.viewItem
                [ T.init "confirmbox"
                    |> T.setValue (Just model.code)
                    |> T.setOnInput (Just InputCode)
                    |> T.setError model.error
                    |> T.setTitle (Just "Engangskode")
                    |> T.setPlaceholder "Skriv inn engangskoden"
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
        , B.init "Send engangskode på nytt"
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
        , Misc.onboardingStart (\_ -> LoggedIn)
        , FirebaseAuth.verifyUserStart (\_ -> LoggedIn)
        ]
