module Page.Shop exposing (Model, Msg(..), init, subscriptions, update, view)

import Base exposing (AppInfo)
import Data.RefData exposing (FareProduct, LangString(..), TariffZone, UserProfile, UserType(..))
import Data.Ticket exposing (Offer, PaymentStatus, PaymentType(..), Reservation)
import Environment exposing (Environment)
import Fragment.Button as Button
import Fragment.Icon as Icon
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Http
import PageUpdater exposing (PageUpdater)
import Process
import Route exposing (Route)
import Service.Misc as MiscService
import Service.Ticket as TicketService
import Set
import Shared exposing (Shared)
import Task
import Time
import Ui.Button exposing (ThemeColor(..), primary)
import Ui.Group
import Ui.Input as Input
import Ui.Message as Message
import Ui.Section as Section
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
          , mainView = Travelers
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


update : Msg -> Environment -> Model -> PageUpdater Model Msg
update msg env model =
    case msg of
        SetProduct product _ ->
            PageUpdater.fromPair
                ( { model | product = product }
                , TaskUtil.doTask FetchOffers
                )

        SetFromZone zone ->
            PageUpdater.fromPair
                ( { model | fromZone = zone }
                , TaskUtil.doTask FetchOffers
                )

        SetToZone zone ->
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

                Err err ->
                    PageUpdater.init { model | offers = Failed "Unable to load offers" }

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
                    PageUpdater.init { model | reservation = Failed "Unable to reserve offers" }

        ReceivePaymentStatus paymentId result ->
            case ( model.reservation, result ) of
                ( Loaded { orderId }, Ok paymentStatus ) ->
                    case paymentStatus.status of
                        "CAPTURE" ->
                            PageUpdater.init { model | reservation = NotLoaded, offers = NotLoaded }
                                |> PageUpdater.addGlobalAction (GA.SetPendingOrder orderId)
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


richBuyButton : Bool -> msg -> Html msg -> Html msg
richBuyButton disabled action content =
    let
        onClick =
            if disabled then
                [ A.class "disabled-button-new" ]

            else
                [ E.onClick action ]
    in
        H.div (A.class "pseudo-button buy-button-new" :: onClick)
            [ content ]


view : Environment -> AppInfo -> Shared -> Model -> Maybe Route -> Html Msg
view _ _ shared model _ =
    H.div [ A.class "page-shop" ]
        [ H.div []
            [ Ui.Group.togglable
                { title = "Reisetype"
                , icon = Just Icon.bus
                , value = Nothing
                , open = False
                , disabled = True
                , onOpenClick = Nothing
                , id = "reisetype"
                }
                []
            , Ui.Group.togglable
                { title = "Reisende"
                , icon = Just Icon.bus
                , value = Nothing
                , open = model.mainView == Travelers
                , disabled = False
                , onOpenClick = Just (ShowView Travelers)
                , id = "reisende"
                }
                [ viewUserProfiles model shared.userProfiles
                ]
            , Ui.Group.togglable
                { title = "Varighet"
                , icon = Just Icon.duration
                , value = Nothing
                , open = model.mainView == Duration
                , disabled = False
                , onOpenClick = Just (ShowView Duration)
                , id = "varighet"
                }
                [ viewProducts model shared.fareProducts
                ]
            , Ui.Group.togglable
                { title = "Gyldig fra og med"
                , icon = Just Icon.ticket
                , value = Nothing
                , open = model.mainView == Start
                , disabled = False
                , onOpenClick = Just (ShowView Start)
                , id = "duration"
                }
                [ viewStart model
                ]
            , Ui.Group.togglable
                { title = "Soner"
                , icon = Just Icon.ticket
                , value = Nothing
                , open = model.mainView == Zones
                , disabled = False
                , onOpenClick = Just (ShowView Zones)
                , id = "zones"
                }
                [ viewZones model shared.tariffZones
                ]
            ]
        , H.div [ A.class "page-shop__summary" ]
            (case model.offers of
                NotLoaded ->
                    [ H.text "" ]

                Loading _ ->
                    [ H.div [ A.style "padding" "20px" ] [ Button.loading ] ]

                Loaded offers ->
                    let
                        disableButtons =
                            case model.reservation of
                                Loading _ ->
                                    True

                                Loaded _ ->
                                    True

                                _ ->
                                    False

                        totalPrice =
                            offers
                                |> List.map (calculateOfferPrice model.users)
                                |> List.sum
                                |> round
                                |> String.fromInt
                    in
                        [ Section.sectionWithOptions
                            { marginBottom = True
                            , marginTop = False
                            }
                            [ Section.sectionHeader "Oppsummering"
                            , Section.sectionGenericItem
                                [ H.div [ A.class "summary-price" ]
                                    [ H.text ("kr " ++ totalPrice ++ ",00")
                                    ]
                                ]
                            , Message.info (H.text "Husk at du må reise med gyldig moderasjonsbevis")
                            ]
                        , Section.sectionWithOptions
                            { marginBottom = True
                            , marginTop = False
                            }
                            [ Ui.Button.primary Secondary_1 "Kjøp med bankkort" disableButtons (Just Icon.creditcard) (BuyOffers Nets)
                            , Ui.Button.primary Secondary_1 "Kjøp med Vipps" disableButtons (Just <| Ui.Button.coloredIcon Icon.vipps) (BuyOffers Vipps)
                            , Ui.Button.tertiary "Avbryt" False (Just Icon.cross) CloseShop
                            ]
                        ]

                Failed error ->
                    [ H.div [] [ H.text error ] ]
            )
        , case model.reservation of
            NotLoaded ->
                H.text ""

            Loading _ ->
                H.p [] [ H.text "Reserving offers..." ]

            Loaded reservation ->
                H.p [] [ H.text <| "Waiting for payment of order " ++ reservation.orderId ]

            Failed error ->
                H.p [] [ H.text error ]
        ]


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
    Input.radioGroup "Velg varighet" <| List.map (viewProduct model) products


viewProduct : Model -> FareProduct -> Html Msg
viewProduct model product =
    let
        isCurrent =
            model.product == product.id
    in
        Input.radio
            { id = product.id
            , title = langString product.name
            , subtitle = Nothing
            , name = "product"
            , checked = isCurrent
            , onCheck = Just <| SetProduct product.id
            }


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
        H.div [ A.class "section-box" ]
            [ H.div [ A.class "section-header" ] [ H.text "Velg soner" ]
            , H.div [ A.class "section-block" ] [ H.select [ E.onInput SetFromZone ] <| List.map (viewZone model.fromZone) sortedZones ]
            , H.div [ A.class "section-block" ] [ H.select [ E.onInput SetToZone ] <| List.map (viewZone model.toZone) sortedZones ]
            , H.div [ A.class "section-block" ] [ H.node "atb-map" [] [] ]
            ]


viewZone : String -> TariffZone -> Html msg
viewZone current zone =
    H.option
        [ A.value zone.id
        , A.selected (current == zone.id)
        ]
        [ H.text <| langString zone.name ]


viewUserProfiles : Model -> List UserProfile -> Html Msg
viewUserProfiles model userProfiles =
    Input.radioGroup "Reisende"
        (userProfiles
            |> List.filter (.userType >> (/=) UserTypeAnyone)
            |> List.map (viewUserProfile model)
        )


viewUserProfile : Model -> UserProfile -> Html Msg
viewUserProfile model userProfile =
    let
        isCurrent =
            List.any (Tuple.first >> (==) userProfile.userType) model.users
    in
        Input.radio
            { id = userTypeAsIdString userProfile.userType
            , title = langString userProfile.name
            , subtitle = Just <| langString userProfile.description
            , name = "userprofile"
            , checked = isCurrent
            , onCheck = Just <| SetUser userProfile.userType
            }


viewOffer : Offer -> Html msg
viewOffer offer =
    H.div []
        [ H.div [] [ H.text offer.offerId ]
        , H.div [] [ H.text offer.travellerId ]
        , H.div []
            [ offer.prices
                |> List.head
                |> Maybe.map (\price -> price.currency ++ " " ++ price.amount)
                |> Maybe.withDefault "No prices"
                |> H.text
            ]
        ]


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
