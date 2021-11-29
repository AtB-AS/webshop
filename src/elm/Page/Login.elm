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
import Ui.ImgThemed
import Ui.Input.Text as T
import Ui.Message as Message
import Ui.PageHeader as PH
import Ui.ScreenReaderText as SR
import Ui.Section
import Util.PhoneNumber
import Util.Validation as V exposing (FormError, ValidationErrors)
import Validate exposing (Valid)
import Base exposing (AppInfo)


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
    | HideInfoStep
    | InputPhone String
    | OnPhoneInputFocus
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
    , showInfoStep : Bool
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
      , showInfoStep = True
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

        HideInfoStep ->
            PageUpdater.init { model | showInfoStep = False }

        InputPhone value ->
            PageUpdater.init { model | phone = value, validationErrors = V.remove PhoneField model.validationErrors }

        OnPhoneInputFocus ->
            PageUpdater.init
                { model
                    | phone =
                        if String.isEmpty model.phone then
                            "+47"

                        else
                            model.phone
                }

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
            validatePhone { model | phone = Util.PhoneNumber.withDefaultCountryCode model.phone }

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
    FirebaseAuth.loginPhone <| Util.PhoneNumber.withDefaultCountryCode phoneNumber


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


view : Environment -> Model -> AppInfo -> Html Msg
view env model appInfo =
    if model.showInfoStep then
        H.div [ A.class "page page--narrow" ]
            [ viewIllustration
            , H.div []
                [ Ui.Section.view
                    [ Ui.Section.viewHeader "Nettbutikken er del av AtBs nye billettsystem. Her er noen ting du bør vite"
                    , Ui.Section.viewItem
                        [ viewInfoPointWithIcon Icon.change
                            "Ny nettbutikk har ingen kobling mot den gamle. Bruk opp gyldige billetter der før du går videre."
                        , viewInfoPointWithIcon Icon.fastTime
                            "Du kan reise straks billetten er betalt – uten å vente på aktivering av t:kort."
                        , viewInfoPointWithIcon Icon.travelCardOutlined
                            "Mista t:kortet? Sletting, bestilling og registrering av t:kort gjør du enkelt selv."
                        , viewInfoPointWithIcon Icon.cloudOutlined
                            "Billetten ligger trygt forvart på din profil – uavhengig av hva som skjer med t:kortet ditt."
                        ]
                    , B.init "Jeg forstår - neste"
                        |> B.setIcon (Just Icon.rightArrow)
                        |> B.setType "button"
                        |> B.setOnClick (Just HideInfoStep)
                        |> B.primary B.Primary_2
                    ]
                , B.init "Les mer om AtBs nye reisetjenester her"
                    |> B.setElement H.a
                    |> B.setAttributes [ A.href "https://www.atb.no/vi-oppgraderer/" ]
                    |> B.link
                , B.init "For our English speaking travellers"
                    |> B.setElement H.a
                    |> B.setAttributes [ A.href appInfo.englishTranslationsUrl ]
                    |> B.link
                ]
            ]

    else
        H.div []
            [ case model.step of
                StepLogin ->
                    Html.Extra.nothing

                StepConfirm ->
                    PH.init
                        |> PH.setBackButton ( "Avbryt", BackLogin )
                        |> PH.view
                        |> List.singleton
                        |> H.div [ A.class "pageLogin__header" ]
            , H.div [ A.class "page page--narrow" ]
                [ viewIllustration
                , case model.step of
                    StepLogin ->
                        viewLogin model

                    StepConfirm ->
                        viewConfirm env model
                , H.node "atb-login-recaptcha" [] []
                ]
            ]


viewIllustration : Html msg
viewIllustration =
    Ui.ImgThemed.view
        [ A.src "/assets/images/travel-illustration.svg"
        , A.class "pageLogin__illustration"
        , A.alt ""
        , A.attribute "role" "presentation"
        ]
        []


viewInfoPointWithIcon : Html msg -> String -> Html msg
viewInfoPointWithIcon icon text =
    H.div [ A.class "pageLogin__infoItem" ]
        [ Icon.viewLargeMonochrome icon
        , H.p [ A.class "pageLogin__infoItem__content" ] [ H.text text ]
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
            [ H.div []
                [ H.p []
                    [ H.text "Logg inn eller opprett en ny profil med engangskode på telefonen din."
                    , SR.onlyRead "Brukere av skjermleser anbefales innlogging med e-post."
                    ]
                ]
            ]
        , Ui.Section.viewItem <| viewPhoneInputs model
        , Ui.Section.viewPaddedItem
            [ H.p [] [ H.a [ Route.href <| Route.Login EmailPath ] [ H.text "Jeg vil heller bruke e-post" ] ]
            ]
        , prerequisitesNotice
        , B.init "Send engangskode"
            |> B.setIcon (Just Icon.rightArrow)
            |> B.setLoading model.loading
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
                [ H.text "Logg inn på din profil med e-post og passord eller "
                , H.a [ Route.href <| Route.Login PhonePath, A.title "Logg på med telefon og engangskode" ] [ H.text "bruk engangskode fra telefonen" ]
                , H.text "."
                ]
            ]
        , Ui.Section.viewItem <| viewEmailInputs "Logg inn med din e-postadresse" "Skriv inn passordet ditt" "current-password" model
        , Ui.Section.viewPaddedItem
            [ H.p [] [ H.a [ Route.href <| Route.Login RegisterEmailPath ] [ H.text "Opprett en ny profil" ] ]
            ]
        , prerequisitesNotice
        , B.init "Logg inn"
            |> B.setIcon (Just Icon.rightArrow)
            |> B.setLoading model.loading
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
        , Ui.Section.viewItem <| viewEmailInputs "Skriv inn din e-postadresse" "Velg et passord" "new-password" model
        , prerequisitesNotice
        , B.init "Opprett profil"
            |> B.setIcon (Just Icon.rightArrow)
            |> B.setLoading model.loading
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
        , prerequisitesNotice
        , B.init "Tilbakestill passord"
            |> B.setIcon (Just Icon.rightArrow)
            |> B.setLoading model.loading
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
        [ H.img [ A.src "/assets/icons/waving-hand.png", A.alt "", A.attribute "role" "presentation" ] []
        , H.text "Velkommen til AtBs nettbutikk"
        ]


prerequisitesNotice : Html msg
prerequisitesNotice =
    H.p []
        [ H.text "I en periode har nettbutikken enkelte forutsetninger. Gjør deg kjent med disse før du logger inn. "
        , H.a [ A.href "https://www.atb.no/vi-oppgraderer", A.target "_blank", A.title "Les mer på AtBs nettside (åpner ny side)" ] [ H.text "Vi oppgraderer (åpner ny side)." ]
        ]
        |> Message.Warning
        |> Message.message


viewPhoneInputs : Model -> List (Html Msg)
viewPhoneInputs model =
    [ Html.Extra.viewMaybe Message.error model.error
    , T.init "phone"
        |> T.setValue (Just model.phone)
        |> T.setOnInput (Just InputPhone)
        |> T.setType "tel"
        |> T.setRequired True
        |> T.setError (V.select PhoneField model.validationErrors)
        |> T.setTitle (Just "Telefonnummer")
        |> T.setPlaceholder "Logg inn med telefonnummeret ditt"
        |> T.setAttributes [ E.onFocus OnPhoneInputFocus ]
        |> T.view
    ]


viewEmailInputs : String -> String -> String -> Model -> List (Html Msg)
viewEmailInputs emailPlaceholder passwordPlaceholder passwordAutocomplete model =
    [ Html.Extra.viewMaybe Message.error model.error
    , T.init "email"
        |> T.setValue (Just model.email)
        |> T.setOnInput (Just InputEmail)
        |> T.setType "email"
        |> T.setRequired True
        |> T.setTitle (Just "E-post")
        |> T.setPlaceholder emailPlaceholder
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
            [ A.attribute "autocomplete" passwordAutocomplete
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
            [ Ui.Section.viewPaddedItem [ H.p [] [ H.text ("Vi har sendt en engangskode til " ++ model.phone) ] ]
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
                |> B.setLoading model.loading
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
