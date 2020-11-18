module Service.Ticket exposing (capture, getTicketList, receipt, reserve, search)

import Data.Ticket exposing (Offer, Price, Reservation, Ticket)
import Environment exposing (Environment)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodeP
import Json.Encode as Encode exposing (Value)
import Url.Builder


{-| Search for offers.
-}
search : Environment -> Http.Request (List Offer)
search env =
    let
        url =
            Url.Builder.absolute
                [ "api", "ticket", "v1", "search" ]
                []

        body =
            Encode.object
                [ ( "products", Encode.list Encode.string [ "ATB:PreassignedFareProduct:61be5f93" ] )
                , ( "travellers", Encode.list encodeTraveller [ 1 ] )
                , ( "zones", Encode.list Encode.string [ "ATB:TariffZone:1" ] )
                ]
    in
        Http.request
            { method = "POST"
            , headers = [ Http.header "Atb-Install-Id" env.installId ]
            , url = url
            , body = Http.jsonBody body
            , expect = Http.expectJson (Decode.list offerDecoder)
            , timeout = Nothing
            , withCredentials = False
            }


{-| Reserve offers.
-}
reserve : Environment -> List ( String, Int ) -> Http.Request Reservation
reserve env offers =
    let
        url =
            Url.Builder.absolute
                [ "api", "ticket", "v1", "reserve" ]
                []

        body =
            Encode.object
                [ ( "customer_id", Encode.string env.installId )
                , ( "offers", Encode.list encodeOffer offers )
                , ( "payment_type", Encode.int 1 )
                ]
    in
        Http.request
            { method = "POST"
            , headers = [ Http.header "Atb-Install-Id" env.installId ]
            , url = url
            , body = Http.jsonBody body
            , expect = Http.expectJson reservationDecoder
            , timeout = Nothing
            , withCredentials = False
            }


{-| Capture an order.
-}
capture : Environment -> Int -> Int -> Http.Request ()
capture env paymentId transactionId =
    let
        url =
            Url.Builder.absolute
                [ "api", "ticket", "v1", "capture" ]
                []

        body =
            Encode.object
                [ ( "payment_id", Encode.int paymentId )
                , ( "transaction_id", Encode.int transactionId )
                ]
    in
        Http.request
            { method = "PUT"
            , headers = [ Http.header "Atb-Install-Id" env.installId ]
            , url = url
            , body = Http.jsonBody body
            , expect = Http.expectStringResponse (\_ -> Ok ())
            , timeout = Nothing
            , withCredentials = False
            }


{-| Get a receipt for an order.
-}
receipt : Environment -> String -> String -> Http.Request ()
receipt env emailAddress orderId =
    let
        url =
            Url.Builder.absolute
                [ "api", "ticket", "v1", "receipt" ]
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


{-| Get list of tickets.
-}
getTicketList : Environment -> String -> Http.Request (List Ticket)
getTicketList env customerId =
    let
        url =
            Url.Builder.absolute
                [ "api", "ticket", "v1", "ticket", env.installId ]
                []
    in
        Http.request
            { method = "GET"
            , headers = [ Http.header "Atb-Install-Id" env.installId ]
            , url = url
            , body = Http.emptyBody
            , expect = Http.expectJson ticketListDecoder
            , timeout = Nothing
            , withCredentials = False
            }



-- INTERNAL


ticketDecoder : Decoder Ticket
ticketDecoder =
    Decode.succeed Ticket
        |> DecodeP.required "duration" Decode.int
        |> DecodeP.required "order_id" Decode.string
        |> DecodeP.required "order_version" Decode.string
        |> DecodeP.required "product_name" Decode.string
        |> DecodeP.required "usage_valid_from" Decode.int
        |> DecodeP.required "usage_valid_to" Decode.int
        |> DecodeP.required "user_profiles" (Decode.list Decode.string)


ticketListDecoder : Decoder (List Ticket)
ticketListDecoder =
    Decode.field "fare_contracts" (Decode.list ticketDecoder)


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
