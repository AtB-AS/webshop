module Ui.HamburgerButton exposing (view)

import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E


view : String -> String -> Bool -> msg -> Html msg
view id menuId open onClick =
    H.button
        [ E.onClick onClick
        , A.classList
            [ ( "ui-hamburgerButton", True )
            , ( "ui-hamburgerButton--open", open )
            ]
        , A.type_ "button"
        , A.attribute "aria-label" "Meny"
        , A.attribute "aria-haspopup" "menu"
        , A.attribute "aria-controls" menuId
        , A.id id
        , A.attribute "aria-expanded" <| boolToString open
        ]
        [ H.span [] []
        , H.span [] []
        , H.span [] []
        ]


boolToString : Bool -> String
boolToString bool =
    if bool then
        "true"

    else
        "false"
