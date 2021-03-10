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
import Util.Status exposing (Status(..))
import Util.Task as TaskUtil


type Msg
    = FetchOffers
    | ReceiveOffers (Result Http.Error (List Offer))
    | BuyOffers PaymentType
    | ReceiveBuyOffers (Result Http.Error Reservation)
    | ReceivePaymentStatus Int (Result Http.Error PaymentStatus)
    | CloseShop
    | SetProduct String
    | SetFromZone String
    | SetToZone String
    | ModUser UserType Int
    | SetUser UserType
    | ShowTravelers
    | ShowDuration
    | ShowStart
    | ShowZones
    | SetTime String
    | SetDate String
    | SetNow
    | GetIsoTime String


type MainView
    = Travelers
    | Duration
    | Start
    | Zones


type alias Model =
    { product : String
    , fromZone : String
    , toZone : String
    , users : List ( UserType, Int )
    , offers : Status (List Offer)
    , reservation : Status Reservation
    , mainView : MainView
    , now : Bool
    , travelDate : String
    , travelTime : String
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
          , travelDate = ""
          , travelTime = "00:00"
          , isoTime = Nothing
          }
        , TaskUtil.doTask FetchOffers
        )


update : Msg -> Environment -> Model -> PageUpdater Model Msg
update msg env model =
    case msg of
        SetProduct product ->
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

        SetUser userType ->
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
            case ( env.customerNumber, model.offers ) of
                ( 0, _ ) ->
                    PageUpdater.init model

                ( customerNumber, Loaded offers ) ->
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
                            , buyOffers env customerNumber paymentType offerCounts
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
            case result of
                Ok paymentStatus ->
                    case paymentStatus.status of
                        "CAPTURE" ->
                            PageUpdater.init { model | reservation = NotLoaded, offers = NotLoaded }
                                |> PageUpdater.addGlobalAction GA.RefreshTickets
                                |> PageUpdater.addGlobalAction GA.CloseShop

                        "CANCEL" ->
                            PageUpdater.init { model | reservation = NotLoaded }

                        _ ->
                            PageUpdater.fromPair ( model, fetchPaymentStatus env paymentId )

                Err _ ->
                    PageUpdater.init { model | reservation = NotLoaded }

        CloseShop ->
            PageUpdater.init model
                |> PageUpdater.addGlobalAction GA.CloseShop

        ShowTravelers ->
            PageUpdater.init { model | mainView = Travelers }

        ShowDuration ->
            PageUpdater.init { model | mainView = Duration }

        ShowStart ->
            PageUpdater.init { model | mainView = Start }

        ShowZones ->
            PageUpdater.init { model | mainView = Zones }

        SetNow ->
            PageUpdater.init { model | now = not model.now }

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

        GetIsoTime isoTime ->
            PageUpdater.fromPair
                ( { model | isoTime = Just isoTime, now = False }
                , TaskUtil.doTask FetchOffers
                )


actionButton : Bool -> msg -> String -> Html msg
actionButton active action title =
    H.div [ A.class "pseudo-button", A.classList [ ( "active", active ) ], E.onClick action ]
        [ H.button [ A.class "action-button" ] [ H.text title ] ]


richActionButton : Bool -> msg -> Html msg -> Html msg
richActionButton active action content =
    H.div [ A.class "pseudo-button", A.classList [ ( "active", active ) ], E.onClick action ]
        [ content ]


view : Environment -> AppInfo -> Shared -> Model -> Maybe Route -> Html Msg
view _ _ shared model _ =
    H.div [ A.class "shop" ]
        [ H.div [ A.class "left" ]
            [ H.div [ A.class "section-box" ]
                [ H.div [] [ H.div [ A.class "disabled-button" ] [ H.text "Reisetype" ] ]
                , actionButton (model.mainView == Travelers) ShowTravelers "Reisende"
                , actionButton (model.mainView == Duration) ShowDuration "Varighet"
                , actionButton (model.mainView == Start) ShowStart "Gyldig fra og med"
                , actionButton (model.mainView == Zones) ShowZones "Soner"
                ]
            ]
        , H.div [ A.class "middle" ]
            [ case model.mainView of
                Travelers ->
                    H.div [] [ viewUserProfiles model shared.userProfiles ]

                Duration ->
                    H.div [] [ viewProducts model shared.fareProducts ]

                Start ->
                    H.div [] [ viewStart model ]

                Zones ->
                    H.div [] [ viewZones model shared.tariffZones ]
            ]
        , H.div [ A.class "right" ]
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
                            List.map
                                (.prices >> List.map .amountFloat >> List.head >> Maybe.withDefault 0.0)
                                offers
                                |> List.sum
                                |> round
                                |> String.fromInt
                    in
                        [ H.div [ A.class "section-box" ]
                            [ H.div [ A.class "section-header" ] [ H.text "Oppsummering" ]
                            , H.div [ A.class "summary-price" ]
                                [ H.text ("kr " ++ totalPrice ++ ",00")
                                ]
                            ]
                        , H.div [ A.class "section-box" ]
                            [ H.div [ A.class "buy-button" ]
                                [ H.button
                                    [ E.onClick <| BuyOffers Nets
                                    , A.disabled disableButtons
                                    ]
                                    [ H.text "Betal med bankkort" ]
                                ]
                            , H.div [ A.class "buy-button" ]
                                [ H.button
                                    [ E.onClick <| BuyOffers Vipps
                                    , A.disabled disableButtons
                                    ]
                                    [ H.text "Betal med Vipps" ]
                                ]
                            , actionButton False CloseShop "Avbryt"
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
            SetNow
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
                , A.type_ "time"
                , A.value model.travelTime
                ]
                []
            ]
        , H.div [ A.class "section-block" ]
            [ H.input
                [ E.onInput SetDate
                , A.type_ "date"
                , A.value model.travelDate
                ]
                []
            ]
        ]


viewProducts : Model -> List FareProduct -> Html Msg
viewProducts model products =
    H.div [ A.class "section-box" ]
        (H.div [ A.class "section-header" ] [ H.text "Velg varighet" ]
            :: List.map (viewProduct model) products
        )


viewProduct : Model -> FareProduct -> Html Msg
viewProduct model product =
    let
        isCurrent =
            model.product == product.id

        extraHtml =
            if isCurrent then
                H.span [ A.style "float" "right" ] [ Icon.checkmark ]

            else
                H.text ""
    in
        richActionButton False
            (SetProduct product.id)
            (H.div [ A.style "display" "flex", A.style "width" "100%" ]
                [ H.span [ A.style "flex-grow" "1" ] [ H.text <| langString product.name ]
                , extraHtml
                ]
            )


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
            , H.div [ A.class "section-block" ] [ H.text "mapbox" ]
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
    H.div [ A.class "section-box" ]
        (H.div [ A.class "section-header" ] [ H.text "Velg reisende" ]
            :: (userProfiles
                    |> List.filter (.userType >> (/=) UserTypeAnyone)
                    |> List.map (viewUserProfile model)
               )
        )


viewUserProfile : Model -> UserProfile -> Html Msg
viewUserProfile model userProfile =
    let
        isCurrent =
            List.any (Tuple.first >> (==) userProfile.userType) model.users

        extraHtml =
            if isCurrent then
                Icon.checkmark

            else
                H.text ""
    in
        richActionButton False
            (SetUser userProfile.userType)
            (H.div [ A.style "display" "flex", A.style "width" "100%" ]
                [ H.span [ A.style "flex-grow" "1" ] [ H.text <| langString userProfile.name ]
                , extraHtml
                ]
            )


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
    MiscService.convertedTime GetIsoTime



-- INTERNAL


fetchOffers : Environment -> String -> String -> String -> List ( UserType, Int ) -> Maybe String -> Cmd Msg
fetchOffers env product fromZone toZone users travelDate =
    [ fromZone, toZone ]
        |> Set.fromList
        |> Set.toList
        |> TicketService.search env travelDate product users
        |> Http.toTask
        |> Task.attempt ReceiveOffers


buyOffers : Environment -> Int -> PaymentType -> List ( String, Int ) -> Cmd Msg
buyOffers env customerNumber paymentType offerCounts =
    offerCounts
        |> TicketService.reserve env customerNumber paymentType
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
