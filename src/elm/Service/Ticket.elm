module Service.Ticket exposing
    ( getPaymentStatus
    , receipt
    , reserve
    , search
    )

import Data.Ticket exposing (Offer, PaymentStatus, PaymentType(..), Price, Reservation, Ticket)
import Environment exposing (Environment)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodeP
import Json.Encode as Encode exposing (Value)
import Url.Builder
import Util.Http as HttpUtil


{-| Search for offers.
-}
search : Environment -> Http.Request (List Offer)
search env =
    let
        url =
            Url.Builder.crossOrigin env.ticketUrl
                [ "ticket", "v1", "search" ]
                []

        body =
            Encode.object
                [ ( "products", Encode.list Encode.string [ "ATB:PreassignedFareProduct:61be5f93" ] )
                , ( "travellers", Encode.list encodeTraveller [ 1 ] )
                , ( "zones", Encode.list Encode.string [ "ATB:TariffZone:1" ] )
                ]
    in
        HttpUtil.post env url (Http.jsonBody body) (Decode.list offerDecoder)


{-| Reserve offers.
-}
reserve : Environment -> Int -> PaymentType -> List ( String, Int ) -> Http.Request Reservation
reserve env customerNumber paymentType offers =
    let
        url =
            Url.Builder.crossOrigin env.ticketUrl
                [ "ticket", "v1", "reserve" ]
                []

        body =
            Encode.object
                [ ( "customer_id", Encode.string env.installId )
                , ( "customer_number", Encode.int customerNumber )
                , ( "offers", Encode.list encodeOffer offers )
                , ( "payment_type", encodePaymentType paymentType )
                , ( "payment_redirect_url", Encode.string "http://127.0.0.1:8080/thanks" )
                ]
    in
        HttpUtil.post env url (Http.jsonBody body) reservationDecoder


{-| Get a receipt for an order.
-}
receipt : Environment -> String -> String -> Http.Request ()
receipt env emailAddress orderId =
    let
        url =
            Url.Builder.crossOrigin env.ticketUrl
                [ "ticket", "v1", "receipt" ]
                []

        body =
            Encode.object
                [ ( "email_address", Encode.string emailAddress )
                , ( "order_id", Encode.string orderId )
                , ( "order_version", Encode.int 1 )
                ]
    in
        Http.request
            { method = "POST"
            , headers = [ Http.header "Atb-Install-Id" env.installId ]
            , url = url
            , body = Http.jsonBody body
            , expect = Http.expectStringResponse (\_ -> Ok ())
            , timeout = Nothing
            , withCredentials = False
            }


getPaymentStatus : Environment -> Int -> Http.Request PaymentStatus
getPaymentStatus env paymentId =
    let
        url =
            Url.Builder.crossOrigin env.ticketUrl
                [ "ticket", "v1", "payments", String.fromInt paymentId ]
                []
    in
        HttpUtil.get env url paymentStatusDecoder



-- INTERNAL


paymentStatusDecoder : Decoder PaymentStatus
paymentStatusDecoder =
    Decode.succeed PaymentStatus
        |> DecodeP.required "order_id" Decode.string
        |> DecodeP.required "status" Decode.string
        |> DecodeP.required "payment_type" Decode.string


encodeTraveller : Int -> Value
encodeTraveller count =
    Encode.object
        [ ( "count", Encode.int count )
        , ( "id", Encode.string "adult" )
        , ( "user_type", Encode.string "ADULT" )
        ]


priceDecoder : Decoder Price
priceDecoder =
    Decode.succeed Price
        |> DecodeP.required "amount" Decode.string
        |> DecodeP.required "amount_float" Decode.float
        |> DecodeP.required "currency" Decode.string


offerDecoder : Decoder Offer
offerDecoder =
    Decode.succeed Offer
        |> DecodeP.required "offer_id" Decode.string
        |> DecodeP.required "prices" (Decode.list priceDecoder)
        |> DecodeP.required "traveller_id" Decode.string


encodeOffer : ( String, Int ) -> Value
encodeOffer ( offerId, count ) =
    Encode.object
        [ ( "offer_id", Encode.string offerId )
        , ( "count", Encode.int count )
        ]


reservationDecoder : Decoder Reservation
reservationDecoder =
    Decode.succeed Reservation
        |> DecodeP.required "order_id" Decode.string
        |> DecodeP.required "payment_id" Decode.int
        |> DecodeP.required "transaction_id" Decode.int
        |> DecodeP.required "url" Decode.string


encodePaymentType : PaymentType -> Value
encodePaymentType paymentType =
    case paymentType of
        Nets ->
            Encode.int 1

        Vipps ->
            Encode.int 2
