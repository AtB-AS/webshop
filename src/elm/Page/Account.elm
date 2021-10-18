module Page.Account exposing (EditSection(..), Model, Msg(..), init, setEditSection, subscriptions, update, view)

import Base exposing (AppInfo)
import Browser.Dom as Dom
import Data.PaymentType as PaymentType exposing (PaymentCard(..), PaymentType(..))
import Data.RefData exposing (Consent)
import Data.Ticket exposing (RecurringPayment)
import Data.Webshop exposing (GivenConsent)
import Dict exposing (Dict)
import Environment exposing (Environment)
import Fragment.Icon as Icon
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Html.Extra
import Http exposing (Error(..))
import Notification
import PageUpdater exposing (PageUpdater)
import Route exposing (Route)
import Service.FirebaseAuth as FirebaseAuth
import Service.Misc as MiscService exposing (Profile, SignInMethod, SignInProvider(..))
import Service.Ticket as TicketService
import Service.Webshop as WebshopService
import Shared exposing (Shared)
import Task
import Time
import Ui.Button as B
import Ui.InlineButtonLink
import Ui.Input.Checkbox as Checkbox
import Ui.Input.EditSection as EditSection
import Ui.Input.MaskedText as MaskedInput
import Ui.Input.Text as Text
import Ui.LoadingText
import Ui.Message as Message
import Ui.Section
import Ui.TextContainer as Text
import Ui.TravelCardText
import Url.Builder as Url
import Util.Maybe as MaybeUtil
import Util.PhoneNumber
import Util.Status exposing (Status(..))
import Util.Task as TaskUtil
import Util.Time as TimeUtil
import Util.TravelCard
import Util.Validation as Validation exposing (FormError, ValidationErrors)
import Validate exposing (Valid)


type EditSection
    = TravelCardSection
    | NameSection
    | EmailSection
    | PhoneSection
    | RecurringPaymentSection Int


type FieldName
    = TravelCard
    | Email
    | PhoneInput
    | FirstName
    | LastName
    | NameFields
    | Consent
    | RecurringPayment Int


type Msg
    = OnEnterPage
    | UpdateFirstName String
    | UpdateLastName String
    | UpdateEmail String
    | UpdatePhone String
    | OnInputPhoneFocus
    | InputTravelCard String
    | StateTravelCard MaskedInput.State
    | ReceiveUpdateProfile (List FieldName) (Result Http.Error ())
    | RemoveTravelCard
    | Logout
    | ProfileChange (Maybe Profile)
    | SaveNames
    | SaveEmail
    | SavePhone
    | ResetPassword String
    | RequestResetPassword
    | SaveTravelCard
    | SetEditSection (Maybe EditSection) (Maybe String)
    | LoadingEditSection (Maybe EditSection)
    | ClearValidationError
    | FocusItem String
    | ResetState
    | NoOp
    | ToggleConsent Int Bool
    | ReceiveUpdateConsent Int (Result Http.Error GivenConsent)
    | FetchConsents
    | ReceiveConsents (Result Http.Error (List GivenConsent))
    | GetRecurringPayments
    | ReceiveRecurringPayments (Result Http.Error (List RecurringPayment))
    | EndRecurringPayment RecurringPayment
    | ReceiveEndRecurringPayment RecurringPayment (Result Http.Error ())
    | UpdateZone Time.Zone


type alias Model =
    { firstName : String
    , lastName : String
    , email : String
    , phone : String
    , travelCard : String
    , travelCardState : MaskedInput.State
    , profile : Maybe Profile
    , givenConsents : Dict Int GivenConsent
    , pendingConsents : Dict Int Bool
    , editSection : Maybe EditSection
    , loadingEditSection : Maybe EditSection
    , validationErrors : ValidationErrors FieldName
    , recurringPayments : Status (List RecurringPayment)
    , timeZone : Time.Zone
    }


init : ( Model, Cmd Msg )
init =
    ( { firstName = ""
      , lastName = ""
      , email = ""
      , phone = ""
      , travelCard = ""
      , travelCardState = MaskedInput.initState
      , profile = Nothing
      , givenConsents = Dict.empty
      , pendingConsents = Dict.empty
      , editSection = Nothing
      , loadingEditSection = Nothing
      , validationErrors = []
      , recurringPayments = NotLoaded
      , timeZone = Time.utc
      }
    , Task.perform UpdateZone Time.here
    )


update : Msg -> Environment -> AppInfo -> Model -> PageUpdater Model Msg
update msg env appInfo model =
    case msg of
        OnEnterPage ->
            let
                focusElement =
                    case model.editSection of
                        Just _ ->
                            Nothing

                        Nothing ->
                            Just "page-header"
            in
                PageUpdater.fromPair ( model, TaskUtil.doTask FetchConsents )
                    |> PageUpdater.addCmd (TaskUtil.doTask GetRecurringPayments)
                    |> (PageUpdater.addGlobalAction <| GA.SetTitle <| Just "Min profil")
                    |> (PageUpdater.addGlobalAction <| GA.FocusItem focusElement)

        ResetState ->
            let
                mapProfileWithDefault s =
                    MaybeUtil.mapWithDefault s "" model.profile
            in
                PageUpdater.init
                    { model
                        | editSection = Nothing
                        , firstName = mapProfileWithDefault .firstName
                        , lastName = mapProfileWithDefault .lastName
                        , email = mapProfileWithDefault .email
                        , phone = mapProfileWithDefault .phone
                        , travelCardState = MaskedInput.initState
                        , travelCard =
                            model.profile
                                |> MaybeUtil.flatMap .travelCard
                                |> MaybeUtil.mapWithDefault (.id >> String.fromInt) ""
                        , loadingEditSection = Nothing
                        , validationErrors = []
                    }

        UpdateFirstName value ->
            PageUpdater.init { model | firstName = value }

        UpdateLastName value ->
            PageUpdater.init { model | lastName = value }

        UpdateEmail value ->
            PageUpdater.init { model | email = value }

        UpdatePhone value ->
            PageUpdater.init { model | phone = value }

        OnInputPhoneFocus ->
            PageUpdater.init
                { model
                    | phone =
                        if String.isEmpty model.phone then
                            "+47"

                        else
                            model.phone
                }

        ResetPassword email ->
            PageUpdater.init model
                |> ("E-post med for å sette nytt passord er sendt til "
                        ++ email
                        |> H.text
                        |> Message.Valid
                        |> Message.message
                        |> (\s -> Notification.setContent s Notification.init)
                        |> GA.ShowNotification
                        |> PageUpdater.addGlobalAction
                   )

        RequestResetPassword ->
            case model.profile of
                Just profile ->
                    PageUpdater.fromPair ( model, resetUsingEmail profile.email )

                Nothing ->
                    PageUpdater.init model

        InputTravelCard value ->
            PageUpdater.init
                { model
                    | travelCard = value
                    , validationErrors = Validation.remove TravelCard model.validationErrors
                }

        StateTravelCard state ->
            PageUpdater.init
                { model | travelCardState = state }

        SaveNames ->
            PageUpdater.fromPair
                ( { model
                    | loadingEditSection = Just NameSection
                    , validationErrors = Validation.removeAll [ FirstName, LastName, NameFields ] model.validationErrors
                  }
                , updateProfile env model.firstName model.lastName
                )

        SavePhone ->
            let
                modelWithCode =
                    { model
                        | phone = Util.PhoneNumber.withDefaultCountryCode model.phone
                    }
            in
                case validatePhone .phone modelWithCode of
                    Ok _ ->
                        PageUpdater.fromPair
                            ( { model
                                | loadingEditSection = Just PhoneSection
                                , validationErrors = Validation.remove PhoneInput model.validationErrors
                              }
                            , updatePhone env modelWithCode.phone
                            )

                    Err errors ->
                        PageUpdater.init { model | validationErrors = errors }

        ReceiveUpdateProfile fields result ->
            case result of
                Ok () ->
                    PageUpdater.init
                        { model
                            | loadingEditSection = Nothing
                            , editSection = Nothing
                            , validationErrors = Validation.removeAll fields model.validationErrors
                        }
                        |> PageUpdater.addGlobalAction
                            (Notification.init
                                |> Notification.setContent (Message.valid "Profilen din ble oppdatert.")
                                |> GA.ShowNotification
                            )

                Err error ->
                    PageUpdater.init
                        { model
                            | loadingEditSection = Nothing
                            , validationErrors = Validation.add fields (Util.TravelCard.serverErrorToString WebshopService.travelCardErrorDecoder error) model.validationErrors
                        }

        ProfileChange (Just profile) ->
            PageUpdater.init
                { model
                    | profile = Just profile
                    , firstName = profile.firstName
                    , lastName = profile.lastName
                    , email = profile.email
                    , phone = profile.phone
                    , travelCard = MaybeUtil.mapWithDefault (.id >> String.fromInt) "" profile.travelCard
                    , validationErrors = []
                }

        ProfileChange Nothing ->
            PageUpdater.init { model | profile = Nothing }

        RemoveTravelCard ->
            PageUpdater.fromPair
                ( { model
                    | loadingEditSection = Just TravelCardSection
                    , validationErrors = Validation.remove TravelCard model.validationErrors
                  }
                , removeTravelCard env model.travelCard
                )

        SaveEmail ->
            case validateEmail .email model of
                Ok _ ->
                    PageUpdater.fromPair
                        ( { model
                            | loadingEditSection = Just EmailSection
                            , validationErrors = Validation.remove Email model.validationErrors
                          }
                        , updateEmail env model.email
                        )

                Err errors ->
                    PageUpdater.init { model | validationErrors = errors }

        SaveTravelCard ->
            case validateTravelCard appInfo.travelCardValidPrefix model of
                Ok _ ->
                    PageUpdater.fromPair
                        ( { model
                            | loadingEditSection = Just TravelCardSection
                            , validationErrors = Validation.remove TravelCard model.validationErrors
                          }
                        , updateTravelCard env model.travelCard
                        )

                Err errors ->
                    PageUpdater.init { model | validationErrors = errors }

        SetEditSection (Just section) focusId ->
            PageUpdater.init { model | editSection = Just section, validationErrors = [] }
                |> PageUpdater.addCmd (focusBox focusId)

        SetEditSection Nothing focusId ->
            case model.profile of
                Nothing ->
                    PageUpdater.init model

                Just profile ->
                    PageUpdater.fromPair
                        ( { model
                            | editSection = Nothing
                            , validationErrors = []
                            , firstName = profile.firstName
                            , lastName = profile.lastName
                            , email = profile.email
                            , travelCard = Maybe.withDefault "" (Maybe.map (.id >> String.fromInt) profile.travelCard)
                          }
                        , focusBox focusId
                        )

        LoadingEditSection section ->
            PageUpdater.init { model | loadingEditSection = section }

        Logout ->
            PageUpdater.init
                { model
                    | firstName = model.firstName
                    , lastName = model.lastName
                    , email = model.email
                    , editSection = Nothing
                    , validationErrors = []
                }
                |> PageUpdater.addGlobalAction GA.Logout

        ClearValidationError ->
            PageUpdater.init { model | validationErrors = [] }

        FocusItem id ->
            PageUpdater.fromPair ( model, focusBox <| Just id )

        NoOp ->
            PageUpdater.init model

        FetchConsents ->
            PageUpdater.fromPair ( model, fetchConsents env )

        ReceiveConsents result ->
            case result of
                Ok givenConsents ->
                    PageUpdater.init
                        { model
                            | givenConsents =
                                givenConsents
                                    |> List.map
                                        (\givenConsent ->
                                            ( givenConsent.consentId, givenConsent )
                                        )
                                    |> Dict.fromList
                            , pendingConsents = Dict.empty
                        }

                Err _ ->
                    -- TODO: There is an issue with this call being made two times when a user
                    -- refreshes and thus we can't handle this properly currently. A refactor of
                    -- the way we handle user data and tokens will fix this.
                    PageUpdater.init model

        ToggleConsent id value ->
            case model.profile of
                Just profile ->
                    -- NOTE: No field for entering specific email for consents, so we are
                    -- using the profile email for now.
                    if String.isEmpty profile.email then
                        model
                            |> addValidationError Consent "E-postadresse må fylles ut først."
                            |> PageUpdater.init

                    else
                        PageUpdater.fromPair
                            ( { model
                                | validationErrors =
                                    Validation.remove Consent model.validationErrors
                                , pendingConsents =
                                    Dict.insert id value model.pendingConsents
                              }
                            , updateConsent env id value profile.email
                            )

                Nothing ->
                    PageUpdater.init model

        ReceiveUpdateConsent consentId result ->
            let
                newModel =
                    { model | pendingConsents = Dict.remove consentId model.pendingConsents }
            in
                case result of
                    Ok givenConsent ->
                        PageUpdater.init
                            { newModel
                                | givenConsents =
                                    Dict.insert consentId givenConsent model.givenConsents
                            }

                    Err _ ->
                        newModel
                            |> addValidationError Consent "Fikk ikke til å lagre samtykke."
                            |> PageUpdater.init

        GetRecurringPayments ->
            PageUpdater.fromPair ( model, getRecurringPayments env )

        ReceiveRecurringPayments result ->
            case result of
                Ok recurringPayments ->
                    PageUpdater.init
                        { model | recurringPayments = Loaded recurringPayments }

                Err _ ->
                    -- TODO: There is an issue with this call being made two times when a user
                    -- refreshes and thus we can't handle this properly currently. A refactor of
                    -- the way we handle user data and tokens will fix this.
                    let
                        errorMessage =
                            "Fikk ikke hentet lagrede betalingskort."
                    in
                        PageUpdater.init { model | recurringPayments = Failed errorMessage }

        EndRecurringPayment recurringPayment ->
            PageUpdater.fromPair
                ( { model
                    | loadingEditSection = Just <| RecurringPaymentSection recurringPayment.id
                    , validationErrors = Validation.remove (RecurringPayment recurringPayment.id) model.validationErrors
                  }
                , endRecurringPayment env recurringPayment
                )

        ReceiveEndRecurringPayment recurringPayment result ->
            let
                id =
                    recurringPayment.id

                notificationText =
                    "Betalingskort " ++ recurringPaymentTitle recurringPayment ++ " ble fjernet."

                newModel =
                    { model | loadingEditSection = Nothing }
            in
                case ( result, model.recurringPayments ) of
                    ( Ok _, Loaded recurringPayments ) ->
                        { newModel
                            | editSection = Nothing
                            , recurringPayments = Loaded (List.filter (.id >> (/=) id) recurringPayments)
                        }
                            |> PageUpdater.init
                            |> PageUpdater.addGlobalAction
                                (Message.valid notificationText
                                    |> (\s -> Notification.setContent s Notification.init)
                                    |> GA.ShowNotification
                                )

                    ( _, _ ) ->
                        newModel
                            |> addValidationError (RecurringPayment id) "Fikk ikke til å fjerne betalingskortet."
                            |> PageUpdater.init

        UpdateZone zone ->
            PageUpdater.init { model | timeZone = zone }


addValidationError : FieldName -> String -> Model -> Model
addValidationError field error model =
    { model | validationErrors = Validation.add [ field ] error model.validationErrors }


validateEmail : (Model -> String) -> Model -> Result (List (FormError FieldName)) (Valid Model)
validateEmail sel model =
    if String.isEmpty model.email then
        Validation.validate Validation.void model

    else
        Validation.validate (Validation.emailValidator Email sel) model


validatePhone : (Model -> String) -> Model -> Result (List (FormError FieldName)) (Valid Model)
validatePhone sel model =
    if String.isEmpty <| sel model then
        Validation.validate Validation.void model

    else
        Validation.validate (Validation.phoneValidator PhoneInput sel) model


validateTravelCard : String -> Model -> Result (List (FormError FieldName)) (Valid Model)
validateTravelCard travelCardPrefix =
    Validation.validate (Validation.travelCardValidator travelCardPrefix TravelCard .travelCard)


focusBox : Maybe String -> Cmd Msg
focusBox id =
    id
        |> Maybe.map (\i -> Task.attempt (\_ -> NoOp) (Dom.focus i))
        |> Maybe.withDefault Cmd.none


view : Environment -> AppInfo -> Shared -> Model -> Maybe Route -> Html Msg
view env appInfo shared model _ =
    H.div [ A.class "page" ]
        [ viewMain env model shared appInfo
        , H.aside [] [ viewSidebar model appInfo ]
        ]


getIdentifier : Maybe MiscService.Profile -> MiscService.SignInProvider -> String
getIdentifier profile selectedProvider =
    profile
        |> Maybe.map .signInMethods
        |> Maybe.map (List.filter (\n -> n.provider == selectedProvider))
        |> Maybe.map (List.map .uid)
        |> Maybe.withDefault []
        |> List.head
        |> Maybe.withDefault ""


viewSidebar : Model -> AppInfo -> Html Msg
viewSidebar model appInfo =
    let
        hasPhoneProvider =
            model.profile
                |> Maybe.map .signInMethods
                |> Maybe.map (List.map .provider)
                |> Maybe.withDefault []
                |> List.member MiscService.Phone

        subject =
            "Slett profilen min"

        body =
            "Jeg ønsker at min profil med all tilhørende informasjon slettes fra nettbutikken og andre tilhørende systemer. "
                ++ (if hasPhoneProvider == True then
                        "Profilen min er tilknyttet følgende telefonnummer: " ++ getIdentifier model.profile MiscService.Phone

                    else
                        "Profilen min er tilknyttet følgende e-post: " ++ getIdentifier model.profile MiscService.Password
                   )
                ++ """

                Jeg forstår at sletting av min profil innebærer følgende:
                
                - Eventuelle fortsatt gyldige eller fremtidige billetter på min profil vil slettes.
                - Jeg får ikke lenger tilgang til billetthistorikk eller kvitteringer.
                - Jeg må legge ved kopi av gyldig identifikasjon i denne e-posten, slik at kundeservice kan verifisere at jeg er eier av profilen.
                """

        deleteLink =
            "mailto:"
                ++ appInfo.supportEmail
                ++ Url.toQuery
                    [ Url.string "body" body
                    , Url.string "subject" subject
                    ]
    in
        Ui.Section.view
            [ B.init "Logg ut"
                |> B.setIcon (Just Icon.logout)
                |> B.setOnClick (Just Logout)
                |> B.tertiary
            , B.init "Slett profil"
                |> B.setIcon (Just Icon.delete)
                |> B.setElement H.a
                |> B.setAttributes [ A.href deleteLink, A.title "Send e-post til kundeservice med telefonnummer eller e-post for å få slettet din profil." ]
                |> B.primary B.Primary_destructive
            ]


viewMain : Environment -> Model -> Shared -> AppInfo -> Html Msg
viewMain env model shared appInfo =
    H.div [ A.class "main" ]
        [ Ui.Section.view
            (case model.profile of
                Just profile ->
                    [ viewProfile env model profile
                    , viewSignIn model profile
                    , viewRecurringPayments model
                    , viewTravelCard appInfo model profile
                    , viewPrivacy model shared appInfo
                    ]

                Nothing ->
                    [ H.p [] [ H.text "No profile." ] ]
            )
        ]


viewProfile : Environment -> Model -> Profile -> Html Msg
viewProfile env model profile =
    let
        onSave =
            Just SaveNames

        onCancel =
            Just <| SetEditSection Nothing Nothing

        loading =
            model.loadingEditSection == Just NameSection
    in
        Ui.Section.viewGroup "Profilinformasjon"
            [ Html.Extra.viewMaybe Message.error (Validation.select NameFields model.validationErrors)
            , EditSection.init
                "Administrer profilinformasjon"
                |> EditSection.setEditButtonType
                    ( "Endre navn", Icon.edit )
                |> EditSection.setOnSave onSave
                |> EditSection.setOnEdit (Just <| SetEditSection (Just NameSection) (Just "firstname"))
                |> EditSection.setInEditMode (fieldInEditMode model.editSection NameSection)
                |> EditSection.setIcon (Just Icon.profileLarge)
                |> EditSection.setButtonGroup
                    (Just <|
                        EditSection.cancelConfirmGroup
                            { onCancel = onCancel
                            , loading = loading
                            }
                    )
                |> EditSection.editSection
                    (\inEditMode ->
                        EditSection.horizontalGroup
                            (if inEditMode then
                                [ Text.init "firstname"
                                    |> Text.setTitle (Just "Fornavn")
                                    |> Text.setError (Validation.select FirstName model.validationErrors)
                                    |> Text.setOnInput (Just <| UpdateFirstName)
                                    |> Text.setPlaceholder "Legg til et fornavn"
                                    |> Text.setValue (Just model.firstName)
                                    |> Text.view
                                , Text.init "lastname"
                                    |> Text.setTitle (Just "Etternavn")
                                    |> Text.setError (Validation.select LastName model.validationErrors)
                                    |> Text.setOnInput (Just <| UpdateLastName)
                                    |> Text.setPlaceholder "Legg til et etternavn"
                                    |> Text.setValue (Just model.lastName)
                                    |> Text.view
                                ]

                             else
                                [ Ui.Section.viewLabelItem "Fornavn" [ viewField identity profile.firstName ]
                                , Ui.Section.viewLabelItem "Etternavn" [ viewField identity profile.lastName ]
                                ]
                            )
                    )
            , Ui.Section.viewWithIconWidthPadding
                [ Ui.Section.viewLabelItem "Kundenummer"
                    [ H.text <| MaybeUtil.mapWithDefault String.fromInt "Fant ikke kundenummer. Ta kontakt med kundeservice" env.customerNumber
                    ]
                ]
            , viewEmailAddress model profile
            , viewPhoneNumber model profile
            ]


viewSignIn : Model -> Profile -> Html Msg
viewSignIn model profile =
    Ui.Section.viewGroup "Innloggingsmetode" <|
        List.map (viewSignInMethod model) profile.signInMethods


viewSignInMethod : Model -> SignInMethod -> Html Msg
viewSignInMethod _ method =
    case method.provider of
        Phone ->
            Ui.Section.viewWithIcon Icon.signInMethodLarge
                [ Ui.Section.viewLabelItem "Engangspassord"
                    [ H.text "Engangspassord på SMS til "
                    , viewField Util.PhoneNumber.format method.uid
                    ]
                ]

        Password ->
            Ui.Section.viewWithIcon Icon.signInMethodLarge
                [ Ui.Section.viewLabelItem "E-post og passord"
                    [ H.text <| "Logg på med e-post " ++ method.uid ++ ". "
                    , Ui.InlineButtonLink.view
                        [ E.onClick RequestResetPassword
                        , A.title <| "Send forespørsel om nytt passord (sender e-post til " ++ method.uid ++ ")"
                        ]
                        [ H.text "Lag nytt passord" ]
                    ]
                ]

        _ ->
            Html.Extra.nothing


viewPhoneNumber : Model -> Profile -> Html Msg
viewPhoneNumber model profile =
    let
        onSave =
            Just SavePhone

        onCancel =
            Just <| SetEditSection Nothing Nothing

        hasEmail =
            profile.phone /= ""

        loading =
            model.loadingEditSection == Just PhoneSection
    in
        if hasProvider profile Phone then
            Html.Extra.nothing

        else
            EditSection.init "Administrer telefonnummer"
                |> EditSection.setEditButtonType
                    (if hasEmail then
                        ( "Endre telefonnummer", Icon.edit )

                     else
                        ( "Legg til telefonnummer", Icon.edit )
                    )
                |> EditSection.setOnSave onSave
                |> EditSection.setOnEdit (Just <| SetEditSection (Just PhoneSection) (Just "phone"))
                |> EditSection.setInEditMode (fieldInEditMode model.editSection PhoneSection)
                |> EditSection.setIcon (Just Icon.phoneLarge)
                |> EditSection.setButtonGroup
                    (Just <|
                        EditSection.cancelConfirmGroup
                            { onCancel = onCancel
                            , loading = loading
                            }
                    )
                |> EditSection.editSection
                    (\inEditMode ->
                        if inEditMode then
                            EditSection.horizontalGroup
                                [ Text.init "phone"
                                    |> Text.setTitle (Just "Telefonnummer")
                                    |> Text.setOnInput (Just <| UpdatePhone)
                                    |> Text.setPlaceholder "Legg til et telefonnummer"
                                    |> Text.setValue (Just model.phone)
                                    |> Text.setType "tel"
                                    |> Text.setRequired False
                                    |> Text.setError (Validation.select PhoneInput model.validationErrors)
                                    |> Text.setAttributes [ E.onFocus OnInputPhoneFocus ]
                                    |> Text.view
                                ]

                        else
                            [ Ui.Section.viewLabelItem "Telefonnummer" [ viewField identity profile.phone ]
                            , model.validationErrors
                                |> Validation.select PhoneInput
                                |> Html.Extra.viewMaybe Message.error
                            ]
                    )


viewEmailAddress : Model -> Profile -> Html Msg
viewEmailAddress model profile =
    let
        onSave =
            Just SaveEmail

        onCancel =
            Just <| SetEditSection Nothing Nothing

        hasEmail =
            profile.email /= ""

        loading =
            model.loadingEditSection == Just EmailSection
    in
        if hasProvider profile Password then
            Html.Extra.nothing

        else
            EditSection.init "Administrer e-post"
                |> EditSection.setEditButtonType
                    (if hasEmail then
                        ( "Endre e-postadresse", Icon.edit )

                     else
                        ( "Legg til e-postadresse", Icon.edit )
                    )
                |> EditSection.setOnSave onSave
                |> EditSection.setOnEdit (Just <| SetEditSection (Just EmailSection) (Just "email"))
                |> EditSection.setInEditMode (fieldInEditMode model.editSection EmailSection)
                |> EditSection.setIcon (Just Icon.emailLarge)
                |> EditSection.setButtonGroup
                    (Just <|
                        EditSection.cancelConfirmGroup
                            { onCancel = onCancel
                            , loading = loading
                            }
                    )
                |> EditSection.editSection
                    (\inEditMode ->
                        if inEditMode then
                            EditSection.horizontalGroup
                                [ Text.init "email"
                                    |> Text.setTitle (Just "E-post")
                                    |> Text.setError (Validation.select Email model.validationErrors)
                                    |> Text.setOnInput (Just <| UpdateEmail)
                                    |> Text.setPlaceholder "Legg til en e-postadresse"
                                    |> Text.setValue (Just model.email)
                                    |> Text.setRequired False
                                    |> Text.setType "email"
                                    |> Text.view
                                ]

                        else
                            [ Ui.Section.viewLabelItem "E-post" [ viewField identity profile.email ]
                            , model.validationErrors
                                |> Validation.select Email
                                |> Html.Extra.viewMaybe Message.error
                            ]
                    )


hasProvider : Profile -> SignInProvider -> Bool
hasProvider profile p =
    profile.signInMethods
        |> List.filter (.provider >> (==) p)
        |> List.isEmpty
        |> not


setEditSection : Maybe EditSection -> Model -> Model
setEditSection editSection model =
    { model | editSection = editSection }


fieldInEditMode : Maybe EditSection -> EditSection -> Bool
fieldInEditMode state actual =
    state == Just actual


viewTravelCard : AppInfo -> Model -> Profile -> Html Msg
viewTravelCard appInfo model profile =
    let
        onSave =
            Just SaveTravelCard

        onRemove =
            Just RemoveTravelCard

        onCancel =
            Just <| SetEditSection Nothing Nothing

        hasTravelCard =
            profile.travelCard /= Nothing

        loading =
            model.loadingEditSection == Just TravelCardSection
    in
        Ui.Section.viewGroup "Reisebevis"
            [ EditSection.init "Administrer reisekort"
                |> EditSection.setEditButtonType
                    (if hasTravelCard then
                        ( "Fjern reisekort", Icon.delete )

                     else
                        ( "Legg til reisekort", Icon.edit )
                    )
                |> EditSection.setIcon (Just Icon.ticketLarge)
                |> EditSection.setOnSave onSave
                |> EditSection.setOnEdit (Just <| SetEditSection (Just TravelCardSection) (Just "tkort"))
                |> EditSection.setInEditMode (fieldInEditMode model.editSection TravelCardSection)
                |> EditSection.setMessage
                    (if hasTravelCard then
                        Nothing

                     else
                        Just <|
                            Message.Warning <|
                                H.text "Ved å registrere et reisekort på profilen din så er det dette du må bruke som reisebevis når du er ute og reiser."
                    )
                |> EditSection.setButtonGroup
                    (if hasTravelCard then
                        Just <|
                            EditSection.destructiveGroup
                                { message = "Er du sikker på at du ønsker å fjerne dette reisekortet? Dette gjør at aktive billetter ikke lengre vil være tilgjengelig via kortet."
                                , confirmLabel = "Fjern reisekort"
                                , onCancel = onCancel
                                , onDestroy = onRemove
                                , loading = loading
                                }

                     else
                        Just <|
                            EditSection.cancelConfirmGroup
                                { onCancel = onCancel
                                , loading = loading
                                }
                    )
                |> EditSection.editSection
                    (\inEditMode ->
                        if inEditMode && not hasTravelCard then
                            EditSection.horizontalGroup
                                [ MaskedInput.init "tkort" InputTravelCard StateTravelCard
                                    |> MaskedInput.setTitle (Just "Reisekortnummer (16-siffer)")
                                    |> MaskedInput.setError (Validation.select TravelCard model.validationErrors)
                                    |> MaskedInput.setPlaceholder "Skriv inn reisekortnummer"
                                    |> MaskedInput.setPattern "#### #### ########"
                                    |> MaskedInput.setAttributes
                                        [ A.autofocus True
                                        , A.attribute "inputmode" "numeric"
                                        ]
                                    |> MaskedInput.view model.travelCardState model.travelCard
                                ]

                        else
                            [ Ui.Section.viewLabelItem "Reisekortnummer"
                                [ case profile.travelCard of
                                    Just travelCard ->
                                        travelCard
                                            |> .id
                                            |> Ui.TravelCardText.view

                                    Nothing ->
                                        H.div [ A.class "pageAccount__noTravelCard" ]
                                            [ Icon.warningColor
                                            , H.p []
                                                [ H.text "Du har ingen reisekort registrert. Om du trenger et nytt fysisk kort kan du "
                                                , H.a
                                                    [ A.href appInfo.orderTravelCardUrl
                                                    , A.target "_blank"
                                                    , A.title "ta kontakt med oss (åpner ny side)."
                                                    ]
                                                    [ H.text "ta kontakt med oss" ]
                                                , H.text "."
                                                ]
                                            ]
                                ]
                            , model.validationErrors
                                |> Validation.select TravelCard
                                |> Html.Extra.viewMaybe Message.error
                            ]
                    )
            ]


viewPrivacy : Model -> Shared -> AppInfo -> Html Msg
viewPrivacy model shared appInfo =
    Ui.Section.viewGroup "Personvern"
        [ Html.Extra.viewIf (not <| List.isEmpty shared.consents) <|
            Ui.Section.viewWithIcon
                Icon.checkmarkCircle
                [ Ui.Section.viewLabelItem "Samtykke"
                    (List.filterMap (viewConsent model) shared.consents)
                ]
        , model.validationErrors
            |> Validation.select Consent
            |> Html.Extra.viewMaybe Message.error
        , Ui.Section.viewPaddedItem
            [ H.p [] [ H.a [ A.href appInfo.privacyDeclarationUrl, A.target "_blank" ] [ H.text "Les vår personvernerklæring (åpner nytt vindu)" ] ]
            ]
        ]


viewConsent : Model -> Consent -> Maybe (Html Msg)
viewConsent model consent =
    Dict.get "nob" consent.title
        |> Maybe.andThen
            (\title ->
                Checkbox.init ("consent" ++ String.fromInt consent.id)
                    |> Checkbox.setChecked
                        (case Dict.get consent.id model.pendingConsents of
                            Just value ->
                                value

                            Nothing ->
                                model.givenConsents
                                    |> Dict.get consent.id
                                    |> MaybeUtil.mapWithDefault .choice False
                        )
                    |> Checkbox.setOnCheck (Just <| ToggleConsent consent.id)
                    |> Checkbox.setTitle title
                    |> Checkbox.view
                    |> Just
            )


viewRecurringPayments : Model -> Html Msg
viewRecurringPayments model =
    Ui.Section.viewGroup "Lagrede betalingskort" <|
        case model.recurringPayments of
            Loaded [] ->
                [ Ui.Section.viewPaddedItem [ H.text "Ingen lagrede betalingskort" ] ]

            Loaded recurringPayments ->
                List.map (viewRecurringPayment model) recurringPayments

            Failed message ->
                [ Ui.Section.viewPaddedItem [ H.text message ] ]

            _ ->
                [ Ui.Section.viewPaddedItem [ Ui.LoadingText.view "1rem" "5rem" ] ]


viewRecurringPayment : Model -> RecurringPayment -> Html Msg
viewRecurringPayment model recurringPayment =
    let
        id =
            recurringPayment.id

        expireString =
            recurringPayment.expiresAt
                |> TimeUtil.isoStringToCardExpirationMonth model.timeZone
                |> Maybe.map (String.append "Utløpsdato ")
                |> Maybe.withDefault ""

        onRemove =
            Just <| EndRecurringPayment recurringPayment

        onCancel =
            Just <| SetEditSection Nothing Nothing

        loading =
            model.loadingEditSection == Just (RecurringPaymentSection id)
    in
        EditSection.init "Administrer betalingskort"
            |> EditSection.setEditButtonType ( "Fjern kort", Icon.delete )
            |> EditSection.setIcon (Just <| PaymentType.toIcon recurringPayment.paymentType)
            |> EditSection.setOnEdit
                (Just <|
                    SetEditSection
                        (Just (RecurringPaymentSection id))
                        Nothing
                )
            |> EditSection.setInEditMode (fieldInEditMode model.editSection (RecurringPaymentSection id))
            |> EditSection.setButtonGroup
                (Just <|
                    EditSection.destructiveGroup
                        { message = "Er du sikker på at du vil fjerne dette kortet?"
                        , confirmLabel = "Fjern kort"
                        , onCancel = onCancel
                        , onDestroy = onRemove
                        , loading = loading
                        }
                )
            |> EditSection.editSection
                (\_ ->
                    [ Ui.Section.viewLabelItem "Bankkort"
                        [ H.div []
                            [ H.p [] [ H.text <| recurringPaymentTitle recurringPayment ]
                            , H.div [ A.class "pageAccount__recurringPayment__expiry" ]
                                [ Text.textContainer H.span (Just Text.SecondaryColor) (Text.Secondary [ H.text expireString ])
                                ]
                            ]
                        ]
                    , model.validationErrors
                        |> Validation.select (RecurringPayment id)
                        |> Html.Extra.viewMaybe Message.error
                    ]
                )


subscriptions : Model -> Sub Msg
subscriptions _ =
    MiscService.onProfileChange ProfileChange



-- INTERNAL


recurringPaymentTitle : RecurringPayment -> String
recurringPaymentTitle recurringPayment =
    PaymentType.format recurringPayment.paymentType ++ ", **** " ++ recurringPayment.maskedPan


{-| Field is valid if it is neither an empty string or an underscore.
-}
hasField : String -> Bool
hasField x =
    x /= "" && x /= "_"


{-| Show the field as normal if it is valid, otherwise say that it's not filled out.
-}
viewField : (String -> String) -> String -> Html msg
viewField fn x =
    if hasField x then
        H.span [] [ H.text <| fn x ]

    else
        H.span
            [ A.style "opacity" "0.5"
            , A.style "font-style" "italic"
            ]
            [ H.text "(ikke utfylt)" ]


updateProfile : Environment -> String -> String -> Cmd Msg
updateProfile env firstName lastName =
    WebshopService.updateProfile env firstName lastName
        |> Http.toTask
        |> Task.attempt (ReceiveUpdateProfile [ NameFields ])


updateEmail : Environment -> String -> Cmd Msg
updateEmail env email =
    WebshopService.updateEmail env email
        |> Http.toTask
        |> Task.attempt (ReceiveUpdateProfile [ Email ])


updatePhone : Environment -> String -> Cmd Msg
updatePhone env phone =
    WebshopService.updatePhone env phone
        |> Http.toTask
        |> Task.attempt (ReceiveUpdateProfile [ PhoneInput ])


updateTravelCard : Environment -> String -> Cmd Msg
updateTravelCard env travelCard =
    travelCard
        |> WebshopService.addTravelCard env
        |> Http.toTask
        |> Task.attempt (ReceiveUpdateProfile [ TravelCard ])


removeTravelCard : Environment -> String -> Cmd Msg
removeTravelCard env travelCard =
    WebshopService.deleteTravelCard env travelCard
        |> Http.toTask
        |> Task.attempt (ReceiveUpdateProfile [ TravelCard ])


resetUsingEmail : String -> Cmd Msg
resetUsingEmail email =
    FirebaseAuth.resetPassword email


updateConsent : Environment -> Int -> Bool -> String -> Cmd Msg
updateConsent env id choice email =
    WebshopService.registerConsent env id choice email
        |> Http.toTask
        |> Task.attempt (ReceiveUpdateConsent id)


fetchConsents : Environment -> Cmd Msg
fetchConsents env =
    WebshopService.getConsents env
        |> Http.toTask
        |> Task.attempt ReceiveConsents


getRecurringPayments : Environment -> Cmd Msg
getRecurringPayments env =
    TicketService.getRecurringPayments env
        |> Http.toTask
        |> Task.attempt ReceiveRecurringPayments


endRecurringPayment : Environment -> RecurringPayment -> Cmd Msg
endRecurringPayment env recurringPayment =
    TicketService.endRecurringPayment env recurringPayment.id
        |> Http.toTask
        |> Task.attempt (ReceiveEndRecurringPayment recurringPayment)
