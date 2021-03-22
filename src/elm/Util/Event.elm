module Util.Event exposing (onEnter)

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
