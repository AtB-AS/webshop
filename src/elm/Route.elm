module Route exposing
    ( Route(..)
    , fromUrl
    , href
    , modifyUrl
    , newUrl
    , routeToString
    )

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as A
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)


type Route
    = Home
    | Shop
    | Thanks
    | History
    | Settings
    | NotFound


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Shop <| s "shop"
        , Parser.map History <| s "history"
        , Parser.map Thanks <| s "thanks"
        , Parser.map Settings <| s "settings"
        ]


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Shop ->
                    [ "shop" ]

                Thanks ->
                    [ "thanks" ]

                History ->
                    [ "history" ]

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
