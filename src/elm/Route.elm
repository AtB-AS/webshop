module Route exposing
    ( Route(..)
    , SettingsRoute(..)
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


type SettingsRoute
    = Overview
    | EditTravelCard


type Route
    = Home
    | Shop
    | History
    | Settings SettingsRoute
    | NotFound


settings : Parser (SettingsRoute -> a) a
settings =
    s "settings"
        </> oneOf
                [ Parser.map Overview Parser.top
                , Parser.map EditTravelCard (s "travelCard")
                ]


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Shop <| s "shop"
        , Parser.map History <| s "history"
        , Parser.map Settings settings
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

                History ->
                    [ "history" ]

                Settings Overview ->
                    [ "settings" ]

                Settings EditTravelCard ->
                    [ "settings", "travelCard" ]

                NotFound ->
                    [ "not-found" ]
    in
        "#/" ++ String.join "/" pieces



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
    Maybe.andThen
        (\fragment ->
            { url | path = Debug.log "fragment" fragment, fragment = Nothing }
                |> Parser.parse parser
                |> Maybe.withDefault NotFound
                |> Just
        )
        url.fragment
