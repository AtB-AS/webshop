module Util.Event exposing (onEnter, stopDefaultEvents)

import Html exposing (Attribute)
import Html.Events as E
import Json.Decode as Decode


onEnter : msg -> Attribute msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg

            else
                Decode.fail "not ENTER"
    in
        E.on "keydown" (Decode.andThen isEnter E.keyCode)


stopDefaultEvents : msg -> Attribute msg
stopDefaultEvents msg =
    msg
        |> Decode.succeed
        |> Decode.map alwaysStopDefaultEvents
        |> E.custom "submit"


alwaysStopDefaultEvents : msg -> { message : msg, stopPropagation : Bool, preventDefault : Bool }
alwaysStopDefaultEvents msg =
    { message = msg, stopPropagation = True, preventDefault = True }
