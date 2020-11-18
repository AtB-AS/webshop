module PageView exposing
    ( PageView
    , init
    , map
    , setDialogs
    , setView
    )

import Dialog exposing (Dialog)
import Html as H exposing (Html)


type alias PageView msg =
    { dialogs : List (Dialog msg)
    , view : Html msg
    }


init : Html msg -> PageView msg
init view =
    { dialogs = []
    , view = view
    }


setView : Html msg -> PageView msg -> PageView msg
setView view pageView =
    { pageView | view = view }


setDialogs : List (Dialog msg) -> PageView msg -> PageView msg
setDialogs dialogs pageView =
    { pageView | dialogs = dialogs }


map : (a -> msg) -> PageView a -> PageView msg
map f pageView =
    { dialogs = List.map (Dialog.map f) pageView.dialogs
    , view = H.map f pageView.view
    }
