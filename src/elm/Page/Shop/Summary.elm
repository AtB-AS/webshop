module Page.Shop.Summary exposing (Model, Msg, Summary, TravellerData, init, makeSummary, subscriptions, update, view)

import Data.PaymentType as PaymentType exposing (PaymentCard(..), PaymentType(..))
import Data.RefData exposing (FareProduct, LangString(..), ProductType(..), UserProfile, UserType(..))
import Data.Ticket exposing (Offer, Reservation)
import Environment exposing (Environment)
import Fragment.Icon as Icon
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Extra
import Http
import List.Extra
import Notification
import Page.Shop.Utils as Utils exposing (TravelDateTime)
import PageUpdater exposing (PageUpdater)
import Service.Misc as MiscService
import Service.Ticket as TicketService
import Shared exposing (Shared)
import Task
import Time
import Ui.Button as B exposing (ThemeColor(..))
import Ui.Input.Radio as Radio
import Ui.LabelItem as LabelItem
import Ui.Message
import Ui.Section as Section
import Util.Format
import Util.PhoneNumber
import Util.Status exposing (Status(..))


type Msg
    = BuyOffers
    | ReceiveBuyOffers (Result Http.Error Reservation)
    | SetPaymentType PaymentType


type alias TravellerData =
    ( String, Int, Float )


type alias Summary =
    { travelMode : String
    , productType : ProductType
    , product : String
    , travellers : String
    , zones : String
    , validFrom : Maybe String
    , validTo : Maybe String
    , travellerData : List TravellerData
    , totalPrice : Float
    , totalVat : Float
    }


type alias OffersQuery =
    { productId : String
    , fromZoneId : String
    , toZoneId : String
    , travelDate : TravelDateTime
    , travelDateEnd : TravelDateTime
    , timeZone : Time.Zone
    }


type alias Model =
    { offers : List Offer
    , query : OffersQuery
    , paymentType : PaymentType
    , reservation : Status Reservation
    }


init : OffersQuery -> List Offer -> Model
init query offers =
    { offers = offers
    , query = query
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
view shared model =
    let
        summary =
            makeSummary model.query model.offers shared
    in
        H.div []
            [ Section.init
                |> Section.setMarginBottom True
                |> Section.viewWithOptions
                    [ Ui.Message.info "I en overgangsperiode kan du oppleve at kortlesere på metrobussholdeplass og i regionbuss avviser t:kortet ditt. Ta det med ro – du kan trygt reise. Ved billettkontroll vil avlesing av t:kort fungere!"
                    ]
            , H.div [ A.class "page page--threeColumns" ]
                [ viewTicketSection summary
                , viewPriceSection summary
                , viewPaymentSection model shared
                ]
            ]


makeSummary : OffersQuery -> List Offer -> Shared -> Summary
makeSummary query offers shared =
    let
        productName =
            nameFromFareProduct shared.fareProducts query.productId

        productType =
            nameTypeFromFareProduct shared.fareProducts query.productId
                |> Maybe.withDefault ProductTypePeriod

        travellerData =
            summerizeOffers shared.userProfiles offers

        price =
            totalPrice offers
    in
        { -- @TODO At some point when expanding to different modes, this should be updated.
          travelMode = "Buss / trikk"
        , productType = productType
        , product = Maybe.withDefault "Ukjent" productName
        , travellers = humanizeTravellerData travellerData
        , zones =
            Maybe.withDefault "Ukjent" <|
                Utils.stringFromZone
                    shared.tariffZones
                    "defaultZone"
                    { fromZone = Just query.fromZoneId
                    , toZone = Just query.toZoneId
                    }
        , validFrom = Utils.stringFromTravelDate query.travelDate query.timeZone
        , validTo = Utils.stringFromTravelDate query.travelDateEnd query.timeZone
        , travellerData = summerizeOffers shared.userProfiles offers
        , totalPrice = price
        , totalVat = vatAmount price shared
        }


summerizeOffers : List UserProfile -> List Offer -> List TravellerData
summerizeOffers userProfiles offers =
    offers
        |> List.Extra.gatherEqualsBy .userType
        |> List.map
            (\( first, others ) ->
                ( Maybe.withDefault "Ukjent" (nameFromUserType userProfiles first.userType)
                , others |> List.length >> (+) 1
                , offerToPrice first + totalPrice others
                )
            )


{-| Humanize and shorten text if more than two traveller groups, making
'2 adults, 1 child, 2 senior' become '5 travellers'.
-}
humanizeTravellerData : List TravellerData -> String
humanizeTravellerData offers =
    if List.length offers > 2 then
        let
            totalTravelers =
                0
        in
            String.fromInt totalTravelers ++ " reisende"

    else
        offers
            |> List.map (\( traveller, count, _ ) -> String.fromInt count ++ " " ++ traveller)
            |> String.join ", "


viewTicketSection : Summary -> Html Msg
viewTicketSection summary =
    Section.view
        [ Section.viewHeader "Om billetten"
        , Section.viewPaddedItem
            [ LabelItem.viewHorizontal "Billettype:" [ H.text <| nameFromProductType summary.productType ]
            , LabelItem.viewHorizontal "Reisetype:" [ H.text <| summary.travelMode ]
            , if summary.productType == ProductTypeCarnet then
                LabelItem.viewHorizontal "Antall billetter:" [ H.text summary.product ]

              else
                LabelItem.viewHorizontal "Periode:" [ H.text summary.product ]
            , LabelItem.viewHorizontal "Reisende:" [ H.text summary.travellers ]
            , LabelItem.viewHorizontal "Sone:" [ H.text summary.zones ]
            , LabelItem.viewHorizontal "Gyldig fra:" [ H.text <| Maybe.withDefault "Nå" summary.validFrom ]
            , Html.Extra.viewMaybe (\validTo -> LabelItem.viewHorizontal "Gyldig til:" [ H.text validTo ]) summary.validTo
            ]
        ]


viewPriceSection : Summary -> Html Msg
viewPriceSection summary =
    let
        price =
            Util.Format.float summary.totalPrice 2

        vat =
            Util.Format.float summary.totalVat 2
    in
        Section.view
            [ Section.viewHeader "Pris"
            , Section.viewPaddedItem
                (List.map viewTravellerData summary.travellerData
                    ++ [ H.hr [ A.class "shopPage__separator" ] []
                       , LabelItem.viewHorizontal
                            "Total:"
                            [ H.p [ A.class "shop__summaryPrice" ]
                                [ H.text price
                                , H.small [] [ H.text "kr" ]
                                ]
                            ]
                       , LabelItem.viewHorizontal "Hvorav mva:"
                            [ H.text <| vat ++ " kr"
                            ]
                       ]
                )
            ]


viewPaymentSection : Model -> Shared -> Html Msg
viewPaymentSection model shared =
    let
        disableButtons =
            case model.reservation of
                Loading _ ->
                    True

                _ ->
                    False

        paymentTypes =
            [ ( "Vipps", Vipps )
            , ( "MasterCard", Nets MasterCard )
            , ( "Visa", Nets Visa )
            , ( "American Express", Nets AmericanExpress )
            ]
    in
        Section.view
            [ Section.viewHeader "Betaling"
            , paymentTypes
                |> List.filter
                    (\( _, paymentType ) ->
                        List.member paymentType shared.paymentTypes
                    )
                |> List.map (paymentTypeRadio model)
                |> Radio.viewLabelGroup "Betalingsmetode"
            , maybeBuyNotice model.offers
            , maybeVippsNotice model shared
            , B.init "Gå til betaling"
                |> B.setDisabled disableButtons
                |> B.setIcon (Just <| Icon.viewMonochrome Icon.rightArrow)
                |> B.setOnClick (Just BuyOffers)
                |> B.primary Primary_2
            ]


paymentTypeRadio : Model -> ( String, PaymentType ) -> Html Msg
paymentTypeRadio model ( title, paymentType ) =
    Radio.init (PaymentType.toString paymentType)
        |> Radio.setTitle title
        |> Radio.setName "paymentType"
        |> Radio.setChecked (model.paymentType == paymentType)
        |> Radio.setOnCheck (Just <| \_ -> SetPaymentType paymentType)
        |> Radio.view


maybeVippsNotice : Model -> Shared -> Html Msg
maybeVippsNotice model shared =
    let
        isVipps =
            model.paymentType == Vipps

        phone =
            shared.profile
                |> Maybe.map .phone
                |> Maybe.withDefault ""

        hasDefaultCountryCode =
            String.isEmpty phone || Util.PhoneNumber.isDefaultCountryCode phone
    in
        if hasDefaultCountryCode || not isVipps then
            Html.Extra.nothing

        else
            Ui.Message.warning "Vipps støtter ikke betaling fra telefonnummer med landskode annet enn +47."


viewTravellerData : TravellerData -> Html Msg
viewTravellerData ( name, count, price ) =
    let
        formattedPrice =
            Util.Format.float price 2
    in
        viewHorizontalItem
            (H.text <| String.fromInt count ++ " " ++ name)
            (H.text <| formattedPrice ++ " kr")


viewHorizontalItem : Html msg -> Html msg -> Html msg
viewHorizontalItem left right =
    H.div [ A.class "summaryPage__horizontal" ]
        [ H.div [] [ left ]
        , H.div [] [ right ]
        ]


subscriptions : Sub Msg
subscriptions =
    Sub.none



-- Helpers


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


nameTypeFromFareProduct : List FareProduct -> String -> Maybe ProductType
nameTypeFromFareProduct products productId =
    products
        |> List.Extra.find (.id >> (==) productId)
        |> Maybe.map .type_


nameFromProductType : ProductType -> String
nameFromProductType productType =
    case productType of
        ProductTypePeriod ->
            "Periodebillett"

        ProductTypeSingle ->
            "Enkeltbillett"

        ProductTypeCarnet ->
            "Klippekort"


langString : LangString -> String
langString (LangString _ value) =
    value


maybeBuyNotice : List Offer -> Html msg
maybeBuyNotice offers =
    let
        reduced =
            offers
                |> List.any (.userType >> hasReducedCost)

        result =
            if reduced then
                Just <| Ui.Message.info "Husk at du må reise med gyldig moderasjonsbevis"

            else
                Nothing

        --
    in
        Html.Extra.viewMaybe identity result


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


totalPrice : List Offer -> Float
totalPrice offers =
    offers
        |> List.map offerToPrice
        |> List.sum
        |> round
        |> toFloat


offerToPrice : Offer -> Float
offerToPrice offer =
    offer.prices
        |> List.map .amountFloat
        |> List.head
        |> Maybe.withDefault 0.0


vatAmount : Float -> Shared -> Float
vatAmount price shared =
    (toFloat shared.remoteConfig.vat_percent / 100) * price



-- IO


buyOffers : Environment -> Maybe String -> PaymentType -> List ( String, Int ) -> Cmd Msg
buyOffers env phone paymentType offerCounts =
    offerCounts
        |> TicketService.reserve env phone paymentType
        |> Http.toTask
        |> Task.attempt ReceiveBuyOffers
