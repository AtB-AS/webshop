module Page.Shop exposing (Model, Msg(..), init, subscriptions, update, view)

import Base exposing (AppInfo)
import Data.RefData exposing (FareProduct, LangString(..), Limitation, TariffZone, UserProfile, UserType(..))
import Data.Ticket exposing (Offer, PaymentStatus, PaymentType(..), Reservation)
import Environment exposing (Environment)
import Fragment.Icon as Icon
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Html.Extra
import Http
import List.Extra
import Notification
import PageUpdater exposing (PageUpdater)
import Process
import Route exposing (Route)
import Service.Misc as MiscService
import Service.Ticket as TicketService
import Set
import Shared exposing (Shared)
import Task
import Time
import Ui.Button as B exposing (ThemeColor(..))
import Ui.Group
import Ui.Input.DropDown as Dropdown exposing (DropDown)
import Ui.Input.Radio as Radio
import Ui.LabelItem
import Ui.LoadingText
import Ui.Message as Message
import Ui.Section as Section
import Util.Format
import Util.Func as Func
import Util.Status exposing (Status(..))
import Util.Task as TaskUtil
import Util.Time as TimeUtil


type Msg
    = FetchOffers
    | ReceiveOffers (Result Http.Error (List Offer))
    | BuyOffers PaymentType
    | ReceiveBuyOffers (Result Http.Error Reservation)
    | ReceivePaymentStatus Int (Result Http.Error PaymentStatus)
    | CloseShop
    | SetProduct String Bool
    | SetFromZone String
    | SetToZone String
    | ModUser UserType Int
    | SetUser UserType Bool
    | ShowView MainView
    | SetTime String
    | SetDate String
    | ToggleNow
    | SetNow Bool
    | UpdateNow Time.Posix
    | UpdateZone Time.Zone
    | GetIsoTime String


type MainView
    = Travelers
    | Duration
    | Start
    | Zones
    | None


type alias Model =
    { product : String
    , fromZone : String
    , toZone : String
    , users : List ( UserType, Int )
    , offers : Status (List Offer)
    , reservation : Status Reservation
    , mainView : MainView
    , now : Bool
    , nowDate : String
    , nowTime : String
    , travelDate : String
    , travelTime : String
    , zone : Time.Zone
    , isoTime : Maybe String
    }


init : Shared -> ( Model, Cmd Msg )
init shared =
    let
        firstZone =
            shared.tariffZones
                |> List.sortWith
                    (\a b ->
                        case ( a.name, b.name ) of
                            ( LangString _ nameA, LangString _ nameB ) ->
                                compare nameA nameB
                    )
                |> List.head
                |> Maybe.map .id
                |> Maybe.withDefault ""
    in
        ( { product =
                shared.fareProducts
                    |> List.head
                    |> Maybe.map .id
                    |> Maybe.withDefault ""
          , fromZone = firstZone
          , toZone = firstZone
          , users = [ ( UserTypeAdult, 1 ) ]
          , offers = NotLoaded
          , reservation = NotLoaded
          , mainView = Duration
          , now = True
          , nowDate = ""
          , nowTime = "00:00"
          , travelDate = ""
          , travelTime = "00:00"
          , zone = Time.utc
          , isoTime = Nothing
          }
        , Cmd.batch
            [ TaskUtil.doTask FetchOffers
            , Task.perform UpdateZone Time.here
            ]
        )


update : Msg -> Environment -> Model -> Shared -> PageUpdater Model Msg
update msg env model shared =
    let
        addGlobalNotification statusText =
            statusText
                |> Message.message
                |> (\s -> Notification.setContent s Notification.init)
                |> GA.ShowNotification
                |> PageUpdater.addGlobalAction
    in
        case msg of
            SetProduct product _ ->
                PageUpdater.fromPair
                    ( { model | product = product, users = maybeResetUsers shared product model.users }
                    , TaskUtil.doTask FetchOffers
                    )

            SetFromZone zone ->
                PageUpdater.fromPair
                    ( { model | fromZone = zone }
                    , TaskUtil.doTask FetchOffers
                    )

            SetToZone zone ->
                let
                    foo =
                        Debug.log "zone" zone
                in
                    PageUpdater.fromPair
                        ( { model | toZone = zone }
                        , TaskUtil.doTask FetchOffers
                        )

            SetUser userType _ ->
                PageUpdater.fromPair
                    ( { model | users = [ ( userType, 1 ) ] }
                    , TaskUtil.doTask FetchOffers
                    )

            ModUser userType change ->
                let
                    users =
                        model.users

                    otherUsers =
                        List.filter (Tuple.first >> (/=) userType) users

                    user =
                        List.filter (Tuple.first >> (==) userType) users
                            |> List.head

                    newValue =
                        case user of
                            Just ( _, count ) ->
                                count + change

                            Nothing ->
                                change

                    newUsers =
                        if newValue < 1 then
                            otherUsers

                        else
                            ( userType, newValue ) :: otherUsers
                in
                    PageUpdater.init { model | users = newUsers }

            FetchOffers ->
                if List.isEmpty model.users then
                    PageUpdater.init model

                else
                    let
                        oldOffers =
                            case model.offers of
                                Loaded offers ->
                                    Just offers

                                Loading offers ->
                                    offers

                                _ ->
                                    Nothing
                    in
                        PageUpdater.fromPair
                            ( { model | offers = Loading oldOffers, reservation = NotLoaded }
                            , fetchOffers env
                                model.product
                                model.fromZone
                                model.toZone
                                model.users
                                (if model.now then
                                    Nothing

                                 else
                                    model.isoTime
                                )
                            )

            ReceiveOffers result ->
                case result of
                    Ok offers ->
                        PageUpdater.init { model | offers = Loaded offers }

                    Err _ ->
                        let
                            errorMessage =
                                "Kunne ikke laste inn billettinformasjon. Prøv igjen."
                        in
                            PageUpdater.init { model | offers = Failed errorMessage }
                                |> addGlobalNotification (Message.Error errorMessage)

            BuyOffers paymentType ->
                case model.offers of
                    Loaded offers ->
                        let
                            offerCounts =
                                List.filterMap
                                    (\offer ->
                                        model.users
                                            |> List.filter (Tuple.first >> (==) offer.userType)
                                            |> List.head
                                            |> Maybe.map (\( _, count ) -> ( offer.offerId, count ))
                                    )
                                    offers
                        in
                            PageUpdater.fromPair
                                ( { model | reservation = Loading Nothing }
                                , buyOffers env paymentType offerCounts
                                )

                    _ ->
                        PageUpdater.init model

            ReceiveBuyOffers result ->
                case result of
                    Ok reservation ->
                        PageUpdater.fromPair
                            ( { model | reservation = Loaded reservation }
                            , Cmd.batch
                                [ MiscService.openWindow reservation.url
                                , fetchPaymentStatus env reservation.paymentId
                                ]
                            )

                    Err _ ->
                        let
                            errorMessage =
                                "Fikk ikke reservert billett. Prøv igjen."
                        in
                            PageUpdater.init { model | reservation = Failed errorMessage }
                                |> addGlobalNotification (Message.Error errorMessage)

            ReceivePaymentStatus paymentId result ->
                case ( model.reservation, model.offers, result ) of
                    ( Loaded reservation, Loaded offers, Ok paymentStatus ) ->
                        case paymentStatus.status of
                            "CAPTURE" ->
                                PageUpdater.init { model | reservation = NotLoaded, offers = NotLoaded }
                                    |> PageUpdater.addGlobalAction
                                        (GA.AddActiveReservation
                                            { reservation = reservation
                                            , offers = offers
                                            , paymentStatus = Just paymentStatus
                                            }
                                        )
                                    |> addGlobalNotification (Message.Valid "Ny billett lagt til.")
                                    |> PageUpdater.addGlobalAction GA.CloseShop

                            "CANCEL" ->
                                PageUpdater.init { model | reservation = NotLoaded }

                            _ ->
                                PageUpdater.fromPair ( model, fetchPaymentStatus env paymentId )

                    _ ->
                        -- Either there was no longer a reservation, or the payment failed. We treat this
                        -- as if the payment was cancelled so the user can try again.
                        PageUpdater.init { model | reservation = NotLoaded }

            CloseShop ->
                PageUpdater.init model
                    |> PageUpdater.addGlobalAction GA.CloseShop

            ShowView mainView ->
                PageUpdater.init (toggleShowMainView model mainView)

            ToggleNow ->
                model
                    |> updateNow (not model.now)
                    |> PageUpdater.init

            SetNow now ->
                model
                    |> updateNow now
                    |> PageUpdater.init

            SetDate date ->
                PageUpdater.fromPair
                    ( { model | travelDate = date, isoTime = Nothing }
                    , MiscService.convertTime ( date, model.travelTime )
                    )

            SetTime time ->
                PageUpdater.fromPair
                    ( { model | travelTime = time, isoTime = Nothing }
                    , MiscService.convertTime ( model.travelDate, time )
                    )

            UpdateNow time ->
                if model.now then
                    model
                        |> updateNowDateTime time
                        |> updateTravelDateTime
                        |> PageUpdater.init

                else
                    model
                        |> updateNowDateTime time
                        |> PageUpdater.init

            UpdateZone zone ->
                PageUpdater.init { model | zone = zone }

            GetIsoTime isoTime ->
                PageUpdater.fromPair
                    ( { model | isoTime = Just isoTime, now = False }
                    , TaskUtil.doTask FetchOffers
                    )


maybeResetUsers : Shared -> String -> List ( UserType, Int ) -> List ( UserType, Int )
maybeResetUsers shared product users =
    let
        data =
            users
                |> List.filter (Tuple.first >> Func.flip List.member (findLimitations product shared.productLimitations))
    in
        if List.isEmpty data then
            [ ( UserTypeAdult, 1 ) ]

        else
            data


toggleShowMainView : Model -> MainView -> Model
toggleShowMainView model mainView =
    { model
        | mainView =
            if model.mainView == mainView then
                None

            else
                mainView
    }


updateNowDateTime : Time.Posix -> Model -> Model
updateNowDateTime time model =
    { model
        | nowDate = TimeUtil.toIsoDate model.zone time
        , nowTime = TimeUtil.toIsoTime model.zone time
    }


updateTravelDateTime : Model -> Model
updateTravelDateTime model =
    { model | travelDate = model.nowDate, travelTime = model.nowTime }


updateNow : Bool -> Model -> Model
updateNow now model =
    if now then
        { model | now = now, travelDate = model.nowDate, travelTime = model.nowTime }

    else
        { model | now = now }


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


view : Environment -> AppInfo -> Shared -> Model -> Maybe Route -> Html Msg
view _ _ shared model _ =
    let
        summary =
            modelSummary shared model

        errorMessage =
            case model.offers of
                Failed message ->
                    Just message

                _ ->
                    Nothing

        disableButtons =
            (errorMessage /= Nothing)
                || (case model.reservation of
                        Loading _ ->
                            True

                        Loaded _ ->
                            True

                        _ ->
                            False
                   )
    in
        H.div [ A.class "page" ]
            [ H.div []
                [ Ui.Group.view
                    { title = "Reisetype"
                    , icon = Just Icon.bus
                    , value = Just "Buss og trikk"
                    , open = False
                    , disabled = True
                    , onOpenClick = Nothing
                    , id = "reisetype"
                    }
                    []
                , Ui.Group.view
                    { title = "Billettype"
                    , icon = Just Icon.duration
                    , value = summary.duration
                    , open = model.mainView == Duration
                    , disabled = False
                    , onOpenClick = Just (ShowView Duration)
                    , id = "varighet"
                    }
                    [ viewProducts model shared.fareProducts ]
                , Ui.Group.view
                    { title = "Reisende"
                    , icon = Just Icon.bus
                    , value =
                        summary.users
                            |> List.head
                            |> Maybe.map (\( name, num ) -> String.fromInt num ++ " " ++ name)
                    , open = model.mainView == Travelers
                    , disabled = False
                    , onOpenClick = Just (ShowView Travelers)
                    , id = "reisende"
                    }
                    [ viewUserProfiles model shared ]
                , Ui.Group.view
                    { title = "Gyldig fra og med"
                    , icon = Just Icon.ticket
                    , value = summary.start
                    , open = model.mainView == Start
                    , disabled = False
                    , onOpenClick = Just (ShowView Start)
                    , id = "duration"
                    }
                    [ viewStart model ]
                , Ui.Group.view
                    { title = "Soner"
                    , icon = Just Icon.ticket
                    , value = summary.zones
                    , open = model.mainView == Zones
                    , disabled = False
                    , onOpenClick = Just (ShowView Zones)
                    , id = "zones"
                    }
                    [ viewZones model shared.tariffZones ]
                ]
            , H.div []
                [ summaryView shared model summary
                , Section.init
                    |> Section.setMarginBottom True
                    |> Section.viewWithOptions
                        [ Html.Extra.viewMaybe Message.error errorMessage
                        , B.init "Kjøp med bankkort"
                            |> B.setDisabled disableButtons
                            |> B.setIcon (Just Icon.creditcard)
                            |> B.setOnClick (Just (BuyOffers Nets))
                            |> B.primary Secondary_1
                        , B.init "Kjøp med Vipps"
                            |> B.setDisabled disableButtons
                            |> B.setIcon (Just (B.coloredIcon Icon.vipps))
                            |> B.setOnClick (Just (BuyOffers Vipps))
                            |> B.primary Secondary_1
                        , B.init "Avbryt"
                            |> B.setDisabled False
                            |> B.setIcon (Just Icon.cross)
                            |> B.setOnClick (Just CloseShop)
                            |> B.tertiary
                        ]
                ]
            ]


nameFromUserType : List UserProfile -> UserType -> Maybe String
nameFromUserType profiles userType =
    profiles
        |> List.Extra.find (.userType >> (==) userType)
        |> Maybe.map (.name >> langString)


nameFromFareProduct : List FareProduct -> String -> Maybe String
nameFromFareProduct products productId =
    products
        |> List.Extra.find (.id >> (==) productId)
        |> Maybe.map (.name >> langString)


stringFromStart : Model -> Maybe String
stringFromStart model =
    if model.now then
        Just "Nå"

    else
        Maybe.andThen (Util.Format.isoStringToFullHumanized model.zone) model.isoTime


stringFromZone : List TariffZone -> Model -> Maybe String
stringFromZone tariffZones model =
    let
        findName zone =
            tariffZones
                |> List.Extra.find (.id >> (==) zone)
                |> Maybe.map (.name >> langString)
                |> Maybe.withDefault "-"

        fromZoneName =
            findName model.fromZone

        toZoneName =
            findName model.toZone
    in
        if model.fromZone == model.toZone then
            Just <| "Reise i 1 sone (" ++ fromZoneName ++ ")"

        else
            Just <| "Reise fra sone " ++ fromZoneName ++ " til sone " ++ toZoneName


type alias ModelSummary =
    { users : List ( String, Int )
    , duration : Maybe String
    , start : Maybe String
    , zones : Maybe String
    }


modelSummary : Shared -> Model -> ModelSummary
modelSummary shared model =
    { users =
        model.users
            |> List.map
                (Tuple.mapFirst (\a -> a |> nameFromUserType shared.userProfiles |> Maybe.withDefault "-"))
    , duration = nameFromFareProduct shared.fareProducts model.product
    , start = stringFromStart model
    , zones = stringFromZone shared.tariffZones model
    }


summaryView : Shared -> Model -> ModelSummary -> Html Msg
summaryView shared model summary =
    let
        errorLoading =
            case model.offers of
                Failed _ ->
                    True

                _ ->
                    False

        totalPrice =
            case model.offers of
                Loaded offers ->
                    offers
                        |> List.map (calculateOfferPrice model.users)
                        |> List.sum
                        |> round
                        |> toFloat
                        |> Just

                _ ->
                    Nothing

        vatAmount =
            Maybe.map ((*) (toFloat shared.remoteConfig.vat_percent / 100)) totalPrice
    in
        Section.init
            |> Section.setMarginBottom True
            |> Section.viewWithOptions
                [ Section.viewHeader "Oppsummering"
                , if errorLoading then
                    Message.error "Fikk ikke lastet pris for denne billetten."

                  else
                    Section.viewPaddedItem
                        [ Ui.LabelItem.viewHorizontal
                            "Total:"
                            [ H.div [ A.class "summary-price" ]
                                [ totalPrice
                                    |> Maybe.map (Func.flip Util.Format.float 2)
                                    |> Maybe.map H.text
                                    |> Maybe.withDefault (Ui.LoadingText.view "1.6875rem" "5rem")
                                , H.small [] [ H.text "kr" ]
                                ]
                            ]
                        , Ui.LabelItem.viewHorizontal "Hvorav mva:"
                            [ vatAmount
                                |> Maybe.map (Func.flip Util.Format.float 2)
                                |> Maybe.map (Func.flip (++) " kr")
                                |> Maybe.map H.text
                                |> Maybe.withDefault (Ui.LoadingText.view "1rem" "3rem")
                            ]
                        ]
                , Section.viewPaddedItem
                    [ Ui.LabelItem.viewCompact "Gyldig fra"
                        [ H.p [] [ H.text <| Maybe.withDefault "" summary.start ]
                        ]
                    ]
                , maybeBuyNotice model.users
                ]


hasReducedCost : UserType -> Bool
hasReducedCost userType =
    case userType of
        UserTypeAdult ->
            False

        UserTypeInfant ->
            False

        UserTypeSchoolPupil ->
            False

        UserTypeAnimal ->
            False

        UserTypeAnyone ->
            False

        _ ->
            True


maybeBuyNotice : List ( UserType, Int ) -> Html msg
maybeBuyNotice users =
    let
        reduced =
            List.any (Tuple.first >> hasReducedCost) users

        result =
            if reduced then
                Just <| Message.info "Husk at du må reise med gyldig moderasjonsbevis"

            else
                Nothing

        --
    in
        Html.Extra.viewMaybe identity result


langString : LangString -> String
langString (LangString _ value) =
    value


viewStart : Model -> Html Msg
viewStart model =
    H.div [ A.class "section-box" ]
        [ H.div [ A.class "section-header" ] [ H.text "Velg starttidspunkt" ]
        , richActionButton False
            (Just ToggleNow)
            (H.div [ A.style "display" "flex", A.style "width" "100%" ]
                [ H.span [ A.style "flex-grow" "1" ] [ H.text "Nå" ]
                , if model.now then
                    Icon.checkmark

                  else
                    H.text ""
                ]
            )
        , H.div [ A.class "section-block" ]
            [ H.input
                [ E.onInput SetTime
                , E.onFocus (SetNow False)
                , A.type_ "time"
                , A.value model.travelTime
                , A.min
                    (if model.travelDate == model.nowDate then
                        model.nowTime

                     else
                        "00:00:00"
                    )
                ]
                []
            ]
        , H.div [ A.class "section-block" ]
            [ H.input
                [ E.onInput SetDate
                , E.onFocus (SetNow False)
                , A.type_ "date"
                , A.value model.travelDate
                , A.min model.nowDate
                ]
                []
            ]
        ]


viewProducts : Model -> List FareProduct -> Html Msg
viewProducts model products =
    products
        |> List.map (viewProduct model)
        |> Radio.viewGroup "Velg billettype"


viewProduct : Model -> FareProduct -> Html Msg
viewProduct model product =
    let
        isCurrent =
            model.product == product.id
    in
        Radio.init product.id
            |> Radio.setTitle (langString product.name)
            |> Radio.setName "product"
            |> Radio.setChecked isCurrent
            |> Radio.setOnCheck (Just <| SetProduct product.id)
            |> Radio.view


viewZones : Model -> List TariffZone -> Html Msg
viewZones model zones =
    let
        sortedZones =
            List.sortWith
                (\a b ->
                    case ( a.name, b.name ) of
                        ( LangString _ nameA, LangString _ nameB ) ->
                            compare nameA nameB
                )
                zones
    in
        Section.viewItem
            [ Dropdown.init
                |> Dropdown.setTitle "Fra sone"
                |> Dropdown.setOnChange (Just SetFromZone)
                |> Dropdown.setOptions (List.map (\item -> ( item.id, langString item.name )) sortedZones)
                |> Dropdown.view
            , Dropdown.init
                |> Dropdown.setTitle "Til sone"
                |> Dropdown.setOnChange (Just SetToZone)
                |> Dropdown.setOptions (List.map (\item -> ( item.id, langString item.name )) sortedZones)
                |> Dropdown.view
            ]


viewZone : String -> TariffZone -> Html msg
viewZone current zone =
    H.option
        [ A.value zone.id
        , A.selected (current == zone.id)
        ]
        [ H.text <| langString zone.name ]


viewUserProfiles : Model -> Shared -> Html Msg
viewUserProfiles model shared =
    shared.userProfiles
        |> List.filter (.userType >> Func.flip List.member (findLimitations model.product shared.productLimitations))
        |> List.filter (.userType >> (/=) UserTypeAnyone)
        |> List.map (viewUserProfile model)
        |> Radio.viewGroup "Reisende"


findLimitations : String -> List Limitation -> List UserType
findLimitations productId fareProducts =
    fareProducts
        |> List.Extra.find (.productId >> (==) productId)
        |> Maybe.map .limitations
        |> Maybe.withDefault []


viewUserProfile : Model -> UserProfile -> Html Msg
viewUserProfile model userProfile =
    let
        isCurrent =
            List.any (Tuple.first >> (==) userProfile.userType) model.users
    in
        Radio.init (userTypeAsIdString userProfile.userType)
            |> Radio.setTitle (langString userProfile.name)
            |> Radio.setName "userprofile"
            |> Radio.setSubtitle (Just <| langString userProfile.description)
            |> Radio.setChecked isCurrent
            |> Radio.setOnCheck (Just <| SetUser userProfile.userType)
            |> Radio.view


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ MiscService.convertedTime GetIsoTime
        , Time.every 1000 UpdateNow
        ]



-- INTERNAL


fetchOffers : Environment -> String -> String -> String -> List ( UserType, Int ) -> Maybe String -> Cmd Msg
fetchOffers env product fromZone toZone users travelDate =
    [ fromZone, toZone ]
        |> Set.fromList
        |> Set.toList
        |> TicketService.search env travelDate product users
        |> Http.toTask
        |> Task.attempt ReceiveOffers


buyOffers : Environment -> PaymentType -> List ( String, Int ) -> Cmd Msg
buyOffers env paymentType offerCounts =
    offerCounts
        |> TicketService.reserve env paymentType
        |> Http.toTask
        |> Task.attempt ReceiveBuyOffers


fetchPaymentStatus : Environment -> Int -> Cmd Msg
fetchPaymentStatus env paymentId =
    Process.sleep 500
        |> Task.andThen
            (\_ ->
                TicketService.getPaymentStatus env paymentId
                    |> Http.toTask
            )
        |> Task.attempt (ReceivePaymentStatus paymentId)


calculateOfferPrice : List ( UserType, Int ) -> Offer -> Float
calculateOfferPrice users offer =
    let
        price =
            offer.prices
                |> List.map .amountFloat
                |> List.head
                |> Maybe.withDefault 0.0

        count =
            users
                |> List.filterMap
                    (\( userType, userCount ) ->
                        if userType == offer.userType then
                            Just userCount

                        else
                            Nothing
                    )
                |> List.head
                |> Maybe.withDefault 0
    in
        price * toFloat count


userTypeAsIdString : UserType -> String
userTypeAsIdString userType =
    case userType of
        UserTypeAdult ->
            "UserTypeAdult"

        UserTypeChild ->
            "UserTypeChild"

        UserTypeInfant ->
            "UserTypeInfant"

        UserTypeSenior ->
            "UserTypeSenior"

        UserTypeStudent ->
            "UserTypeStudent"

        UserTypeYoungPerson ->
            "UserTypeYoungPerson"

        UserTypeSchoolPupil ->
            "UserTypeSchoolPupil"

        UserTypeMilitary ->
            "UserTypeMilitary"

        UserTypeDisabled ->
            "UserTypeDisabled"

        UserTypeDisabledCompanion ->
            "UserTypeDisabledCompanion"

        UserTypeJobSeeker ->
            "UserTypeJobSeeker"

        UserTypeEmployee ->
            "UserTypeEmployee"

        UserTypeAnimal ->
            "UserTypeAnimal"

        UserTypeAnyone ->
            "UserTypeAnyone"
