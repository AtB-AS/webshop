module Page.ShopCarnet exposing (Model, Msg(..), init, subscriptions, update, view)

import Base exposing (AppInfo)
import Data.RefData exposing (FareProduct, LangString(..), Limitation, ProductType(..), TariffZone, UserProfile, UserType(..))
import Data.Ticket exposing (Offer, PaymentType(..), Reservation)
import Environment exposing (Environment)
import Fragment.Icon as Icon
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Http
import List.Extra
import Notification
import Page.Shop.Summary as SummaryPage
import PageUpdater exposing (PageUpdater)
import Process
import Route exposing (Route)
import Service.Misc as MiscService
import Service.Ticket as TicketService
import Set
import Shared exposing (Shared)
import Task
import Ui.Button as B exposing (ThemeColor(..))
import Ui.Group
import Ui.Input.Radio as Radio
import Ui.Input.Select as Select
import Ui.LabelItem
import Ui.LoadingText
import Ui.Message as Message
import Ui.PageHeader as PH
import Ui.Section as Section
import Util.Format
import Util.Func as Func
import Util.Status exposing (Status(..))
import Util.Task as TaskUtil


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
    | CloseSummary
    | GoToSummary
    | SummarySubMsg SummaryPage.Msg


type MainView
    = Travelers
    | Duration
    | Zones
    | None


type alias Model =
    { product : Maybe String
    , fromZone : Maybe String
    , toZone : Maybe String
    , users : List ( UserType, Int )
    , offers : Status (List Offer)
    , reservation : Status Reservation
    , mainView : MainView
    , summary : Maybe SummaryPage.Model
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
      , summary = Nothing
      }
    , TaskUtil.doTask FetchOffers
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
                PageUpdater.init { model | summary = Nothing }

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

                        availableProducts =
                            shared.fareProducts
                                |> List.filter (.type_ >> (==) ProductTypeCarnet)

                        ( firstZone, defaultProduct ) =
                            defaultDerivedData shared availableProducts

                        dataNotLoadedYet =
                            List.isEmpty availableProducts && List.isEmpty shared.tariffZones

                        newProduct =
                            Maybe.withDefault defaultProduct model.product

                        newFromZone =
                            Maybe.withDefault firstZone model.fromZone

                        newToZone =
                            Maybe.withDefault firstZone model.toZone
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
                                    Nothing
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

            CloseSummary ->
                PageUpdater.init { model | summary = Nothing }

            GoToSummary ->
                case model.offers of
                    Loaded offers ->
                        PageUpdater.init
                            { model
                                | summary =
                                    Just <|
                                        SummaryPage.init
                                            { productId = Maybe.withDefault "" model.product
                                            , fromZoneId = Maybe.withDefault "" model.fromZone
                                            , toZoneId = Maybe.withDefault "" model.toZone
                                            , travelDate = Nothing
                                            }
                                            offers
                            }

                    _ ->
                        PageUpdater.init model

            SummarySubMsg subMsg ->
                case model.summary of
                    Nothing ->
                        PageUpdater.init model

                    Just summary ->
                        SummaryPage.update subMsg env summary shared
                            |> PageUpdater.map (\newModel -> { model | summary = Just newModel }) SummarySubMsg


defaultDerivedData : Shared -> List FareProduct -> ( String, String )
defaultDerivedData shared products =
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
            products
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
        availableProducts =
            shared.fareProducts
                |> List.filter (.type_ >> (==) ProductTypeCarnet)

        ( defaultZone, defaultProduct ) =
            defaultDerivedData shared availableProducts

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
        case model.summary of
            Just summaryModel ->
                H.div []
                    [ PH.init
                        |> PH.setTitle (Just "Oppsummering")
                        |> PH.setBackButton (Just ( "Tilbake", E.onClick CloseSummary ))
                        |> PH.view
                    , SummaryPage.view shared summaryModel
                        |> H.map SummarySubMsg
                    ]

            _ ->
                H.div []
                    [ PH.init
                        |> PH.setTitle (Just "Kjøp nytt klippekort")
                        |> PH.setBackButton (Just ( "Avbryt", E.onClick CloseShop ))
                        |> PH.view
                    , H.div [ A.class "page" ]
                        [ Section.view
                            [ Ui.Group.view
                                { title = "Reisetype"
                                , icon = Icon.bus
                                , value = Just "Buss og trikk"
                                , open = False
                                , readonly = True
                                , onOpenClick = Nothing
                                , id = "reisetype"
                                , editTextSuffix = "billett"
                                }
                                []
                            , Ui.Group.view
                                { title = "Antall billetter"
                                , icon = Icon.tickets
                                , value = summary.product
                                , open = False
                                , readonly = True
                                , onOpenClick = Nothing
                                , id = "product"
                                , editTextSuffix = "antall"
                                }
                                []
                            , Ui.Group.view
                                { title = "Reisende"
                                , icon = Icon.bus
                                , value =
                                    summary.users
                                        |> List.head
                                        |> Maybe.map (\( name, num ) -> String.fromInt num ++ " " ++ name)
                                , open = model.mainView == Travelers
                                , readonly = False
                                , onOpenClick = Just (ShowView Travelers)
                                , id = "reisende"
                                , editTextSuffix = "reisende"
                                }
                                [ viewUserProfiles defaultProduct model shared ]
                            , Ui.Group.view
                                { title = "Soner"
                                , icon = Icon.map
                                , value = summary.zones
                                , open = model.mainView == Zones
                                , readonly = False
                                , onOpenClick = Just (ShowView Zones)
                                , id = "zones"
                                , editTextSuffix = "sone"
                                }
                                [ viewZones model defaultZone shared.tariffZones ]
                            ]
                        , H.div []
                            [ summaryView shared model disableButtons
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
    , product : Maybe String
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
        , product = nameFromFareProduct shared.fareProducts product
        , start = Just ""
        , zones = stringFromZone shared.tariffZones defaultZone model
        }


summaryView : Shared -> Model -> Bool -> Html Msg
summaryView shared model disableButtons =
    let
        emptyOffers =
            case model.offers of
                Loaded offers ->
                    List.isEmpty offers

                _ ->
                    False

        summary =
            case model.offers of
                Loaded offers ->
                    Just <|
                        SummaryPage.makeSummary
                            { productId = Maybe.withDefault "" model.product
                            , fromZoneId = Maybe.withDefault "" model.fromZone
                            , toZoneId = Maybe.withDefault "" model.toZone
                            , travelDate = Nothing
                            }
                            offers
                            shared

                _ ->
                    Nothing
    in
        Section.init
            |> Section.viewWithOptions
                [ if emptyOffers then
                    Message.warning "Finner ingen tilgjengelige billetter."

                  else
                    Section.viewPaddedItem
                        [ Ui.LabelItem.viewHorizontal
                            "Total:"
                            [ H.p [ A.class "shop__summaryPrice" ]
                                [ summary
                                    |> Maybe.map .totalPrice
                                    |> Maybe.map (Func.flip Util.Format.float 2)
                                    |> Maybe.map H.text
                                    |> Maybe.withDefault (Ui.LoadingText.view "1.6875rem" "5rem")
                                , H.small [] [ H.text "kr" ]
                                ]
                            ]
                        , Ui.LabelItem.viewHorizontal "Hvorav mva:"
                            [ summary
                                |> Maybe.map .totalVat
                                |> Maybe.map (Func.flip Util.Format.float 2)
                                |> Maybe.map (Func.flip (++) " kr")
                                |> Maybe.map H.text
                                |> Maybe.withDefault (Ui.LoadingText.view "1rem" "3rem")
                            ]
                        ]
                , B.init "Gå til oppsummering"
                    |> B.setDisabled disableButtons
                    |> B.setIcon (Just <| Icon.viewMonochrome Icon.rightArrow)
                    |> B.setOnClick (Just GoToSummary)
                    |> B.primary Primary_2
                ]


langString : LangString -> String
langString (LangString _ value) =
    value


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
    SummaryPage.subscriptions
        |> Sub.map SummarySubMsg



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
