module Page.Shop exposing (Model, Msg(..), init, subscriptions, update, view)

import Base exposing (AppInfo)
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


type alias Model =
    { offers : Status (List Offer)
    , reservation : Status Reservation
    }


init : ( Model, Cmd Msg )
init =
    ( { offers = NotLoaded
      , reservation = NotLoaded
      }
    , TaskUtil.doTask FetchOffers
    )


update : Msg -> Environment -> Model -> PageUpdater Model Msg
update msg env model =
    case msg of
        FetchOffers ->
            PageUpdater.fromPair
                ( { model | offers = Loading Nothing, reservation = NotLoaded }
                , fetchOffers env
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
                    PageUpdater.fromPair
                        ( { model | reservation = Loading Nothing }
                        , buyOffers env customerNumber paymentType offers
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


view : Environment -> AppInfo -> Model -> Maybe Route -> Html Msg
view _ _ model _ =
    H.div [ A.class "box" ]
        [ H.h2 [] [ H.text "Shop" ]
        , H.button [ E.onClick FetchOffers ] [ H.text "Search" ]
        , H.button [ E.onClick CloseShop ] [ H.text "Close" ]
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
                        [ if List.isEmpty offers then
                            H.div [] [ H.text "No offers" ]

                          else
                            H.ol [] <| List.map viewOffer offers
                        , H.div []
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
