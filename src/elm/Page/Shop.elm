module Page.Shop exposing (Model, Msg(..), init, subscriptions, update, view)

import Base exposing (AppInfo)
import Data.Ticket exposing (Offer, PaymentStatus, PaymentType(..), Reservation)
import Environment exposing (Environment)
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
import Task
import Util.Task as TaskUtil


type Msg
    = FetchOffers
    | ReceiveOffers (Result Http.Error (List Offer))
    | BuyOffers PaymentType
    | ReceiveBuyOffers (Result Http.Error Reservation)
    | ReceivePaymentStatus Int (Result Http.Error PaymentStatus)
    | CloseShop


type alias Model =
    { offers : List Offer
    , reservation : Maybe Reservation
    }


init : ( Model, Cmd Msg )
init =
    ( { offers = []
      , reservation = Nothing
      }
    , TaskUtil.doTask FetchOffers
    )


update : Msg -> Environment -> Model -> PageUpdater Model Msg
update msg env model =
    case msg of
        FetchOffers ->
            PageUpdater.fromPair ( model, fetchOffers env )

        ReceiveOffers result ->
            case result of
                Ok offers ->
                    PageUpdater.init { model | offers = offers }

                Err err ->
                    PageUpdater.init model

        BuyOffers paymentType ->
            case env.customerNumber of
                0 ->
                    PageUpdater.init model

                customerNumber ->
                    PageUpdater.fromPair ( model, buyOffers env customerNumber paymentType model.offers )

        ReceiveBuyOffers result ->
            case result of
                Ok reservation ->
                    PageUpdater.fromPair
                        ( { model | reservation = Just reservation }
                        , Cmd.batch
                            [ MiscService.openWindow reservation.url
                            , fetchPaymentStatus env reservation.paymentId
                            ]
                        )

                Err _ ->
                    PageUpdater.init model

        ReceivePaymentStatus paymentId result ->
            case result of
                Ok paymentStatus ->
                    case paymentStatus.status of
                        "CAPTURE" ->
                            PageUpdater.init { model | reservation = Nothing, offers = [] }
                                |> PageUpdater.addGlobalAction GA.RefreshTickets
                                |> PageUpdater.addGlobalAction GA.CloseShop

                        "CANCEL" ->
                            PageUpdater.init { model | reservation = Nothing }

                        _ ->
                            PageUpdater.fromPair ( model, fetchPaymentStatus env paymentId )

                Err _ ->
                    PageUpdater.init { model | reservation = Nothing }

        CloseShop ->
            PageUpdater.init model
                |> PageUpdater.addGlobalAction GA.CloseShop


view : Environment -> AppInfo -> Model -> Maybe Route -> Html Msg
view _ _ model _ =
    H.div [ A.class "box" ]
        [ H.h2 [] [ H.text "Shop" ]
        , H.button [ E.onClick FetchOffers ] [ H.text "Search" ]
        , H.button [ E.onClick CloseShop ] [ H.text "Close" ]
        , H.ol [] <| List.map viewOffer model.offers
        , if List.length model.offers > 0 then
            H.div []
                [ H.button [ E.onClick <| BuyOffers Nets ] [ H.text "Buy with Nets" ]
                , H.button [ E.onClick <| BuyOffers Vipps ] [ H.text "Buy with Vipps" ]
                ]

          else
            H.text ""
        , case model.reservation of
            Just reservation ->
                H.p [] [ H.text <| "Waiting for NETS with order" ++ reservation.orderId ]

            Nothing ->
                H.text ""
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


fetchOffers : Environment -> Cmd Msg
fetchOffers env =
    TicketService.search env
        |> Http.toTask
        |> Task.attempt ReceiveOffers


buyOffers : Environment -> Int -> PaymentType -> List Offer -> Cmd Msg
buyOffers env customerNumber paymentType offers =
    offers
        |> List.map (\offer -> ( offer.offerId, 1 ))
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
