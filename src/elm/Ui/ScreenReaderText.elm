module Ui.ScreenReaderText exposing (makeSpellable, onlyRead, onlyView, readAndView)

import Html as H exposing (Html)
import Html.Attributes as A


onlyRead : String -> Html msg
onlyRead text =
    H.span [ A.class "ui-srOnly" ] [ H.text text ]


onlyView : String -> Html msg
onlyView text =
    H.span [ A.attribute "aria-hidden" "true" ] [ H.text text ]


readAndView : String -> String -> List (Html msg)
readAndView readText viewText =
    [ onlyRead readText, onlyView viewText ]


makeSpellable : String -> String
makeSpellable text =
    String.join " " <| String.split "" text
