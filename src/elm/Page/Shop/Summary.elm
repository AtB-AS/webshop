module Page.Shop.Summary exposing (Model, Msg, Summary, init, subscriptions, update, view)

import Data.RefData exposing (UserType(..))
import Data.Ticket exposing (Offer, PaymentType(..), Reservation)
import Environment exposing (Environment)
import Fragment.Icon as Icon
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
import Http
import Notification
import PageUpdater exposing (PageUpdater)
import Service.Misc as MiscService
import Service.Ticket as TicketService
import Shared exposing (Shared)
import Task
import Ui.Button as B exposing (ThemeColor(..))
import Ui.Input.Radio as Radio
import Ui.Message
import Ui.Section as Section
import Util.Status exposing (Status(..))


type Msg
    = BuyOffers
    | ReceiveBuyOffers (Result Http.Error Reservation)
    | SetPaymentType PaymentType


type alias Summary =
    { product : String }


type alias Model =
    { offers : List Offer
    , paymentType : PaymentType
    , reservation : Status Reservation
    }


init : List Offer -> Model
init offers =
    { offers = offers
    , paymentType = Vipps
    , reservation = NotLoaded
    }


update : Msg -> Environment -> Model -> Shared -> PageUpdater Model Msg
update msg env model shared =
    let
        addGlobalNotification statusText =
            statusText
                |> Ui.Message.message
                |> (\s -> Notification.setContent s Notification.init)
                |> GA.ShowNotification
                |> PageUpdater.addGlobalAction
    in
        case msg of
            BuyOffers ->
                let
                    offerCounts =
                        List.map
                            (\offer -> ( offer.offerId, 1 ))
                            model.offers

                    phone =
                        Maybe.map .phone shared.profile
                in
                    PageUpdater.fromPair
                        ( { model | reservation = Loading Nothing }
                        , buyOffers env phone model.paymentType offerCounts
                        )

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
                                |> addGlobalNotification (Ui.Message.Error <| H.text errorMessage)

            SetPaymentType paymentType ->
                PageUpdater.init { model | paymentType = paymentType }


view : Shared -> Model -> Html Msg
view _ model =
    let
        disableButtons =
            case model.reservation of
                Loading _ ->
                    True

                _ ->
                    False
    in
        H.div [ A.class "page page--threeColumns" ]
            [ Section.view
                [ Section.viewHeader "Om billetten"
                ]
            , Section.view
                [ Section.viewHeader "Pris"
                ]
            , Section.view
                [ Section.viewHeader "Betaling"
                , Section.viewLabelItem "Betalingsmetode"
                    [ Radio.viewGroup "Betalingsmetode"
                        [ Radio.init "vipps"
                            |> Radio.setTitle "Vipps"
                            |> Radio.setName "paymentType"
                            |> Radio.setChecked (model.paymentType == Vipps)
                            |> Radio.setOnCheck (Just <| \_ -> SetPaymentType Vipps)
                            |> Radio.view
                        , Radio.init "visa"
                            |> Radio.setTitle "Bankkort"
                            |> Radio.setName "paymentType"
                            |> Radio.setChecked (model.paymentType == Nets)
                            |> Radio.setOnCheck (Just <| \_ -> SetPaymentType Nets)
                            |> Radio.view
                        ]
                    ]
                , B.init "Gå til oppsummering"
                    |> B.setDisabled disableButtons
                    |> B.setIcon (Just <| Icon.viewMonochrome Icon.rightArrow)
                    |> B.setOnClick (Just BuyOffers)
                    |> B.primary Primary_2
                ]
            ]


subscriptions : Sub Msg
subscriptions =
    Sub.none



--


buyOffers : Environment -> Maybe String -> PaymentType -> List ( String, Int ) -> Cmd Msg
buyOffers env phone paymentType offerCounts =
    offerCounts
        |> TicketService.reserve env phone paymentType
        |> Http.toTask
        |> Task.attempt ReceiveBuyOffers
