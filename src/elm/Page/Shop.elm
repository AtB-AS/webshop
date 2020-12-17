module Page.Shop exposing (Model, Msg(..), init, subscriptions, update, view)

import Base exposing (AppInfo)
import Data.RefData exposing (FareProduct, LangString(..), TariffZone, UserProfile, UserType(..))
import Data.Ticket exposing (Offer, PaymentStatus, PaymentType(..), Reservation)
import Environment exposing (Environment)
import Fragment.Button as Button
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


type alias Model =
    { product : String
    , fromZone : String
    , toZone : String
    , users : List ( UserType, Int )
    , offers : Status (List Offer)
    , reservation : Status Reservation
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
          , users = []
          , offers = NotLoaded
          , reservation = NotLoaded
          }
        , TaskUtil.doTask FetchOffers
        )


update : Msg -> Environment -> Model -> PageUpdater Model Msg
update msg env model =
    case msg of
        SetProduct product ->
            PageUpdater.init { model | product = product }

        SetFromZone zone ->
            PageUpdater.init { model | fromZone = zone }

        SetToZone zone ->
            PageUpdater.init { model | toZone = zone }

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
                        , fetchOffers env model.product model.fromZone model.toZone model.users
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


view : Environment -> AppInfo -> Shared -> Model -> Maybe Route -> Html Msg
view _ _ shared model _ =
    H.div [ A.class "box" ]
        [ H.h2 [] [ H.text "Shop" ]
        , H.button [ E.onClick FetchOffers ] [ H.text "Search" ]
        , H.button [ E.onClick CloseShop ] [ H.text "Close" ]
        , H.div [] [ viewProducts model shared.fareProducts ]
        , H.div [] [ viewZones model shared.tariffZones ]
        , H.div [] [ viewUserProfiles model shared.userProfiles ]
        , case model.offers of
            NotLoaded ->
                H.text ""

            Loading _ ->
                H.div [ A.style "padding" "20px" ] [ Button.loading ]

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
                in
                    H.div []
                        [ H.div []
                            [ H.button
                                [ E.onClick <| BuyOffers Nets
                                , A.disabled disableButtons
                                ]
                                [ H.text "Buy with Nets" ]
                            , H.button
                                [ E.onClick <| BuyOffers Vipps
                                , A.disabled disableButtons
                                ]
                                [ H.text "Buy with Vipps" ]
                            ]
                        ]

            Failed error ->
                H.div [] [ H.text error ]
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


viewProducts : Model -> List FareProduct -> Html Msg
viewProducts model products =
    H.p []
        [ H.text "Choose product: "
        , H.select [ E.onInput SetProduct ] <| List.map (viewProduct model) products
        ]


viewProduct : Model -> FareProduct -> Html msg
viewProduct model product =
    H.option
        [ A.value product.id
        , A.selected (model.product == product.id)
        ]
        [ H.text <| langString product.name ]


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
        H.p []
            [ H.text "From "
            , H.select [ E.onInput SetFromZone ] <| List.map (viewZone model.fromZone) sortedZones
            , H.text " to "
            , H.select [ E.onInput SetToZone ] <| List.map (viewZone model.toZone) sortedZones
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
    H.div []
        [ userProfiles
            |> List.filter (.userType >> (/=) UserTypeAnyone)
            |> List.map (viewUserProfile model)
            |> H.table [ A.class "shop-user" ]
        ]


viewUserProfile : Model -> UserProfile -> Html Msg
viewUserProfile model userProfile =
    let
        value =
            model.users
                |> List.filter (Tuple.first >> (==) userProfile.userType)
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.withDefault 0

        offer =
            case model.offers of
                Loaded offers ->
                    offers
                        |> List.filter (.userType >> (==) userProfile.userType)
                        |> List.head

                Loading (Just offers) ->
                    offers
                        |> List.filter (.userType >> (==) userProfile.userType)
                        |> List.head

                _ ->
                    Nothing

        onClick change =
            E.onClick <| ModUser userProfile.userType change
    in
        H.tr []
            [ H.td []
                [ if value > 0 then
                    H.button [ onClick -1 ] [ H.text "-" ]

                  else
                    H.text ""
                ]
            , H.td []
                [ H.text <|
                    if value > 0 then
                        String.fromInt value

                    else
                        ""
                ]
            , H.td [] [ H.button [ onClick 1 ] [ H.text "+" ] ]
            , H.td [] [ H.text <| langString userProfile.name ]
            , H.td []
                [ offer
                    |> Maybe.andThen
                        (\{ prices } ->
                            prices
                                |> List.head
                                |> Maybe.map (\{ amountFloat } -> String.fromInt (round amountFloat * value) ++ ",-")
                        )
                    |> Maybe.withDefault ""
                    |> H.text
                ]
            ]


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
    Sub.none



-- INTERNAL


fetchOffers : Environment -> String -> String -> String -> List ( UserType, Int ) -> Cmd Msg
fetchOffers env product fromZone toZone users =
    [ fromZone, toZone ]
        |> Set.fromList
        |> Set.toList
        |> TicketService.search env product users
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
