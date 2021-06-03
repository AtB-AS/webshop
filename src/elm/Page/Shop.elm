module Page.Shop exposing (Model, Msg(..), init, subscriptions, update, view)

import Base exposing (AppInfo)
import Data.RefData exposing (FareProduct, LangString(..), Limitation, TariffZone, UserProfile, UserType(..))
import Data.Ticket exposing (Offer, PaymentType(..), Reservation)
import Environment exposing (Environment)
import Fragment.Icon as Icon
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
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
import Time exposing (Posix)
import Ui.Button as B exposing (ThemeColor(..))
import Ui.Group
import Ui.Input.Radio as Radio
import Ui.Input.Select as Select
import Ui.Input.Text as Text
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
    = OnEnterPage
    | OnLeavePage
    | ResetState
    | FetchOffers
    | ReceiveOffers (Result Http.Error (List Offer))
    | BuyOffers PaymentType
    | ReceiveBuyOffers (Result Http.Error Reservation)
    | CloseShop
    | SetProduct String Bool
    | SetFromZone String
    | SetToZone String
    | ModUser UserType Int
    | SetUser UserType Bool
    | ShowView MainView
    | SetTime String
    | SetDate String
    | UpdateNow Time.Posix
    | UpdateZone Time.Zone
    | GetIsoTime String
    | SetTravelDateTime TravelDateTime


type MainView
    = Travelers
    | Duration
    | Start
    | Zones
    | None


type TravelDateTime
    = TravelNow
    | TravelFuture (Maybe String)


type alias Model =
    { product : Maybe String
    , fromZone : Maybe String
    , toZone : Maybe String
    , users : List ( UserType, Int )
    , offers : Status (List Offer)
    , reservation : Status Reservation
    , mainView : MainView
    , travelDateTime : TravelDateTime
    , now : Posix
    , inputTravelDate : String
    , inputTravelTime : String
    , timeZone : Time.Zone
    }


init : ( Model, Cmd Msg )
init =
    ( { product = Nothing
      , fromZone = Nothing
      , toZone = Nothing
      , users = [ ( UserTypeAdult, 1 ) ]
      , offers = NotLoaded
      , reservation = NotLoaded
      , mainView = Duration
      , travelDateTime = TravelNow
      , now = Time.millisToPosix 0
      , inputTravelDate = ""
      , inputTravelTime = ""
      , timeZone = Time.utc
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
            ResetState ->
                PageUpdater.init <| Tuple.first init

            OnEnterPage ->
                PageUpdater.fromPair ( model, Tuple.second init )
                    |> (Just "Kjøp billett"
                            |> GA.SetTitle
                            |> PageUpdater.addGlobalAction
                       )

            OnLeavePage ->
                PageUpdater.init model

            SetProduct product _ ->
                PageUpdater.fromPair
                    ( { model | product = Just product, users = maybeResetUsers shared product model.users }
                    , TaskUtil.doTask FetchOffers
                    )

            SetFromZone zone ->
                PageUpdater.fromPair
                    ( { model | fromZone = Just zone }
                    , TaskUtil.doTask FetchOffers
                    )

            SetToZone zone ->
                PageUpdater.fromPair
                    ( { model | toZone = Just zone }
                    , TaskUtil.doTask FetchOffers
                    )

            SetUser userType _ ->
                PageUpdater.fromPair
                    ( { model | users = [ ( userType, 1 ) ] }
                    , TaskUtil.doTask FetchOffers
                    )

            SetTravelDateTime (TravelFuture maybeFuture) ->
                case maybeFuture of
                    Nothing ->
                        let
                            travelDate =
                                if String.isEmpty model.inputTravelDate then
                                    TimeUtil.toIsoDate model.timeZone model.now

                                else
                                    model.inputTravelDate

                            travelTime =
                                if String.isEmpty model.inputTravelTime then
                                    TimeUtil.toHoursAndMinutes model.timeZone <| TimeUtil.addHours 1 model.now

                                else
                                    model.inputTravelTime
                        in
                            PageUpdater.fromPair
                                ( { model
                                    | travelDateTime = TravelFuture Nothing
                                    , inputTravelDate = travelDate
                                    , inputTravelTime = travelTime
                                  }
                                , MiscService.convertTime ( travelDate, travelTime )
                                )

                    future ->
                        PageUpdater.fromPair
                            ( { model | travelDateTime = TravelFuture <| future }
                            , TaskUtil.doTask FetchOffers
                            )

            SetTravelDateTime TravelNow ->
                PageUpdater.fromPair
                    ( { model | travelDateTime = TravelNow }
                    , TaskUtil.doTask FetchOffers
                    )

            ModUser userType change ->
                let
                    users =
                        model.users

                    otherUsers =
                        List.filter (Tuple.first >> (/=) userType) users

                    user =
                        users
                            |> List.filter (Tuple.first >> (==) userType)
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

                        ( firstZone, defaultProduct ) =
                            defaultDerivedData shared

                        dataNotLoadedYet =
                            List.isEmpty shared.availableFareProducts && List.isEmpty shared.tariffZones

                        newProduct =
                            Maybe.withDefault defaultProduct model.product

                        newFromZone =
                            Maybe.withDefault firstZone model.fromZone

                        newToZone =
                            Maybe.withDefault firstZone model.toZone

                        travelTime =
                            case model.travelDateTime of
                                TravelFuture (Just time) ->
                                    Just time

                                _ ->
                                    Nothing
                    in
                        if dataNotLoadedYet then
                            PageUpdater.fromPair
                                ( model
                                , Process.sleep 500 |> Task.attempt (\_ -> FetchOffers)
                                )

                        else
                            PageUpdater.fromPair
                                ( { model
                                    | offers = Loading oldOffers
                                    , reservation = NotLoaded
                                    , product = Just newProduct
                                    , fromZone = Just newFromZone
                                    , toZone = Just newToZone
                                  }
                                , fetchOffers env
                                    newProduct
                                    newFromZone
                                    newToZone
                                    model.users
                                    travelTime
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
                                |> addGlobalNotification (Message.Error <| H.text errorMessage)

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

                            phone =
                                Maybe.map .phone shared.profile
                        in
                            PageUpdater.fromPair
                                ( { model | reservation = Loading Nothing }
                                , buyOffers env phone paymentType offerCounts
                                )

                    _ ->
                        PageUpdater.init model

            ReceiveBuyOffers result ->
                case result of
                    Ok reservation ->
                        PageUpdater.fromPair
                            ( { model | reservation = Loaded reservation }
                            , MiscService.navigateTo reservation.url
                            )

                    Err _ ->
                        let
                            errorMessage =
                                "Fikk ikke reservert billett. Prøv igjen."
                        in
                            PageUpdater.init { model | reservation = Failed errorMessage }
                                |> addGlobalNotification (Message.Error <| H.text errorMessage)

            CloseShop ->
                PageUpdater.fromPair ( model, TaskUtil.doTask ResetState )
                    |> PageUpdater.addGlobalAction (GA.RouteTo Route.Home)

            ShowView mainView ->
                PageUpdater.init (toggleShowMainView model mainView)

            SetDate date ->
                PageUpdater.fromPair
                    ( { model | inputTravelDate = date }
                    , MiscService.convertTime ( date, model.inputTravelTime )
                    )

            SetTime time ->
                PageUpdater.fromPair
                    ( { model | inputTravelTime = time }
                    , MiscService.convertTime ( model.inputTravelDate, time )
                    )

            UpdateNow now ->
                PageUpdater.init { model | now = now }

            UpdateZone zone ->
                PageUpdater.init { model | timeZone = zone }

            GetIsoTime isoTime ->
                PageUpdater.fromPair
                    ( model
                    , TaskUtil.doTask <| SetTravelDateTime <| TravelFuture (Just isoTime)
                    )


defaultDerivedData : Shared -> ( String, String )
defaultDerivedData shared =
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

        defaultProduct =
            shared.availableFareProducts
                |> List.head
                |> Maybe.map .id
                |> Maybe.withDefault ""
    in
        ( firstZone, defaultProduct )


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


view : Environment -> AppInfo -> Shared -> Model -> Maybe Route -> Html Msg
view _ _ shared model _ =
    let
        ( defaultZone, defaultProduct ) =
            defaultDerivedData shared

        summary =
            modelSummary ( defaultZone, defaultProduct ) shared model

        errorMessage =
            case model.offers of
                Failed message ->
                    Just message

                _ ->
                    Nothing

        emptyOffers =
            case model.offers of
                Loaded offers ->
                    List.isEmpty offers

                _ ->
                    True

        disableButtons =
            (errorMessage /= Nothing)
                || emptyOffers
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
                    [ viewProducts model defaultProduct shared.availableFareProducts ]
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
                    [ viewUserProfiles defaultProduct model shared ]
                , Ui.Group.view
                    { title = "Gyldig fra og med"
                    , icon = Just Icon.ticket
                    , value = summary.start
                    , open = model.mainView == Start
                    , disabled = False
                    , onOpenClick = Just (ShowView Start)
                    , id = "duration"
                    }
                    (viewStart model)
                , Ui.Group.view
                    { title = "Soner"
                    , icon = Just Icon.ticket
                    , value = summary.zones
                    , open = model.mainView == Zones
                    , disabled = False
                    , onOpenClick = Just (ShowView Zones)
                    , id = "zones"
                    }
                    [ viewZones model defaultZone shared.tariffZones ]
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
    case model.travelDateTime of
        TravelNow ->
            Just "Kjøpstidspunkt"

        TravelFuture (Just time) ->
            TimeUtil.isoStringToFullHumanized model.timeZone time

        _ ->
            Nothing


stringFromZone : List TariffZone -> String -> Model -> Maybe String
stringFromZone tariffZones defaultZone model =
    let
        findName zone =
            tariffZones
                |> List.Extra.find (.id >> (==) zone)
                |> Maybe.map (.name >> langString)
                |> Maybe.withDefault "-"

        fromZoneName =
            findName (Maybe.withDefault defaultZone model.fromZone)

        toZoneName =
            findName (Maybe.withDefault defaultZone model.toZone)
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


modelSummary : ( String, String ) -> Shared -> Model -> ModelSummary
modelSummary ( defaultZone, defaultProduct ) shared model =
    let
        product =
            Maybe.withDefault defaultProduct model.product
    in
        { users =
            model.users
                |> List.map
                    (Tuple.mapFirst (\a -> a |> nameFromUserType shared.userProfiles |> Maybe.withDefault "-"))
        , duration = nameFromFareProduct shared.fareProducts product
        , start = stringFromStart model
        , zones = stringFromZone shared.tariffZones defaultZone model
        }


summaryView : Shared -> Model -> ModelSummary -> Html Msg
summaryView shared model _ =
    let
        emptyOffers =
            case model.offers of
                Loaded offers ->
                    List.isEmpty offers

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

        validFrom =
            case model.travelDateTime of
                TravelNow ->
                    H.text "Kjøpstidspunkt"

                TravelFuture (Just time) ->
                    TimeUtil.isoStringToFullHumanized model.timeZone time
                        |> Maybe.map
                            (\dateTime ->
                                H.time [ A.datetime time ] [ H.text <| dateTime ]
                            )
                        |> Maybe.withDefault Html.Extra.nothing

                TravelFuture Nothing ->
                    Html.Extra.nothing
    in
        Section.init
            |> Section.setMarginBottom True
            |> Section.viewWithOptions
                [ Section.viewHeader "Oppsummering"
                , if emptyOffers then
                    Message.warning "Finner ingen tilgjengelige billetter."

                  else
                    Section.viewPaddedItem
                        [ Ui.LabelItem.viewHorizontal
                            "Total:"
                            [ H.p [ A.class "shop__summaryPrice" ]
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
                        [ validFrom
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


viewStart : Model -> List (Html Msg)
viewStart model =
    let
        isFutureSelected =
            model.travelDateTime /= TravelNow
    in
        [ Radio.viewGroup "Velg avreisetid"
            [ Radio.init "travel-now"
                |> Radio.setTitle "Kjøpstidspunkt"
                |> Radio.setName "traveltime"
                |> Radio.setChecked (not isFutureSelected)
                |> Radio.setOnCheck (Just <| \_ -> SetTravelDateTime TravelNow)
                |> Radio.view
            , Radio.init "travel-future"
                |> Radio.setTitle "Velg dato og tid"
                |> Radio.setName "traveltime"
                |> Radio.setChecked isFutureSelected
                |> Radio.setOnCheck (Just <| \_ -> SetTravelDateTime <| TravelFuture Nothing)
                |> Radio.view
            ]
        , Html.Extra.viewIf isFutureSelected <|
            Section.viewHorizontalGroup
                [ Text.init "date"
                    |> Text.setTitle (Just "Dato")
                    |> Text.setOnInput (Just SetDate)
                    |> Text.setAttributes [ A.min <| TimeUtil.toIsoDate model.timeZone model.now ]
                    |> Text.setType "date"
                    |> Text.setValue (Just model.inputTravelDate)
                    |> Text.view
                , Text.init "time"
                    |> Text.setTitle (Just "Tid")
                    |> Text.setOnInput (Just SetTime)
                    |> Text.setAttributes
                        [ A.min <| TimeUtil.toHoursAndMinutes model.timeZone model.now ]
                    |> Text.setType "time"
                    |> Text.setValue (Just model.inputTravelTime)
                    |> Text.view
                ]
        ]


viewProducts : Model -> String -> List FareProduct -> Html Msg
viewProducts model defaultProduct products =
    products
        |> List.map (viewProduct model defaultProduct)
        |> Radio.viewGroup "Velg billettype"


viewProduct : Model -> String -> FareProduct -> Html Msg
viewProduct model defaultProduct product =
    let
        selectedProduct =
            Maybe.withDefault defaultProduct model.product

        isCurrent =
            selectedProduct == product.id
    in
        Radio.init product.id
            |> Radio.setTitle (langString product.name)
            |> Radio.setName "product"
            |> Radio.setChecked isCurrent
            |> Radio.setOnCheck (Just <| SetProduct product.id)
            |> Radio.view


viewZones : Model -> String -> List TariffZone -> Html Msg
viewZones model defaultZone zones =
    let
        sortedZones =
            List.sortWith
                (\a b ->
                    case ( a.name, b.name ) of
                        ( LangString _ nameA, LangString _ nameB ) ->
                            compare nameA nameB
                )
                zones

        selectedFromZone =
            Maybe.withDefault defaultZone model.fromZone

        selectedToZone =
            Maybe.withDefault defaultZone model.toZone
    in
        Section.viewItem
            [ Section.viewHorizontalGroup
                [ Select.init "travelFromZone"
                    |> Select.setTitle (Just "Avreisesone")
                    |> Select.setOnInput (Just SetFromZone)
                    |> Select.view (List.map (viewZone selectedFromZone) sortedZones)
                , Select.init "travelToZone"
                    |> Select.setTitle (Just "Ankomstsone")
                    |> Select.setOnInput (Just SetToZone)
                    |> Select.view (List.map (viewZone selectedToZone) sortedZones)
                ]
            , Section.viewPaddedItem [ H.p [] [ H.a [ A.href "https://atb.no/soner", A.target "_blank" ] [ H.text "Se sonekart og beskrivelser (åpner ny side)" ] ] ]
            ]


viewZone : String -> TariffZone -> Html msg
viewZone current zone =
    H.option
        [ A.value zone.id
        , A.selected (current == zone.id)
        ]
        [ H.text <| langString zone.name ]


viewUserProfiles : String -> Model -> Shared -> Html Msg
viewUserProfiles defaultProduct model shared =
    let
        product =
            Maybe.withDefault defaultProduct model.product
    in
        shared.userProfiles
            |> List.filter (.userType >> Func.flip List.member (findLimitations product shared.productLimitations))
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


buyOffers : Environment -> Maybe String -> PaymentType -> List ( String, Int ) -> Cmd Msg
buyOffers env phone paymentType offerCounts =
    offerCounts
        |> TicketService.reserve env phone paymentType
        |> Http.toTask
        |> Task.attempt ReceiveBuyOffers


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
