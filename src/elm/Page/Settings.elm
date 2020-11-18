module Page.Settings exposing (Model, Msg, init, subscriptions, update, view)

import Base exposing (AppInfo)
import Environment exposing (Environment)
import Html as H exposing (Html)
import Route exposing (Route)


type Msg
    = NoOp


type alias Model =
    { name : String }


init : ( Model, Cmd Msg )
init =
    ( Model "MyName", Cmd.none )


update : Msg -> Environment -> Model -> ( Model, Cmd Msg )
update msg _ model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Environment -> AppInfo -> Model -> Maybe Route -> Html Msg
view _ _ model _ =
    H.div [] [ H.text <| "Hello from Settings, " ++ model.name ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
