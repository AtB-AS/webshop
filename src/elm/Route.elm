module Route exposing
    ( LoginMethodPath(..)
    , ResponseCode(..)
    , Route(..)
    , fromUrl
    , href
    , modifyUrl
    , newUrl
    , routeToString
    )

import Browser.Navigation as Nav
import Dict
import Html exposing (Attribute)
import Html.Attributes as A
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s)
import Url.Parser.Query as QueryParser


type LoginMethodPath
    = PhonePath
    | EmailPath
    | RegisterEmailPath
    | ForgotPasswordPath


type Route
    = Home
    | Shop
    | ShopCarnet
    | Payment PaymentResponseQuery
    | History
    | Settings
    | Login LoginMethodPath
    | NotFound


type alias PaymentResponseQuery =
    { transactionId : Maybe Int
    , paymentId : Maybe Int
    , orderId : Maybe String
    , responseCode : Maybe ResponseCode
    }


type ResponseCode
    = Cancel
    | OK


paymentResponseQueryParser : QueryParser.Parser PaymentResponseQuery
paymentResponseQueryParser =
    QueryParser.map4 PaymentResponseQuery
        (QueryParser.int "transaction_id")
        (QueryParser.int "payment_id")
        (QueryParser.string "order_id")
        (QueryParser.enum "response_code" <|
            Dict.fromList
                [ ( "Cancel", Cancel )
                , ( "OK", OK )
                ]
        )


parseLoginMethod : Parser (LoginMethodPath -> a) a
parseLoginMethod =
    oneOf
        [ Parser.map PhonePath Parser.top
        , Parser.map EmailPath <| s "email"
        , Parser.map ForgotPasswordPath <| s "forgot-password"
        , Parser.map RegisterEmailPath <| s "register"
        ]


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Shop <| s "shop"
        , Parser.map ShopCarnet <| s "shop-carnet"
        , Parser.map History <| s "history"
        , Parser.map Payment <| s "payment" <?> paymentResponseQueryParser
        , Parser.map Settings <| s "settings"
        , Parser.map Login <| s "login" </> parseLoginMethod
        ]


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Payment _ ->
                    []

                Shop ->
                    [ "shop" ]

                ShopCarnet ->
                    [ "shop-carnet" ]

                History ->
                    [ "history" ]

                Login EmailPath ->
                    [ "login", "email" ]

                Login ForgotPasswordPath ->
                    [ "login", "forgot-password" ]

                Login RegisterEmailPath ->
                    [ "login", "register" ]

                Login PhonePath ->
                    [ "login" ]

                Settings ->
                    [ "settings" ]

                NotFound ->
                    [ "not-found" ]
    in
        "/" ++ String.join "/" pieces



-- PUBLIC HELPERS --


href : Route -> Attribute msg
href route =
    A.href (routeToString route)


modifyUrl : Nav.Key -> Route -> Cmd msg
modifyUrl key route =
    routeToString route |> Nav.replaceUrl key


newUrl : Nav.Key -> Route -> Cmd msg
newUrl key route =
    routeToString route |> Nav.pushUrl key


fromUrl : Url -> Maybe Route
fromUrl url =
    url
        |> Parser.parse parser
        |> Maybe.withDefault NotFound
        |> Just
