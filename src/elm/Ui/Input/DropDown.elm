module Ui.Input.DropDown exposing
    ( DropDown
    , addOption
    , init
    , setOnChange
    , setOptions
    , setTitle
    , view
    )

import Html as H exposing (Html)
import Html.Attributes as A
import Html.Attributes.Extra
import Html.Events as E
import Json.Decode as Json
import Ui.LabelItem


type alias DropDown msg =
    { title : String
    , defaultValue : String
    , options : List ( String, String )
    , onChange : Maybe (String -> msg)
    }


init : DropDown msg
init =
    { title = ""
    , defaultValue = ""
    , options = []
    , onChange = Nothing
    }


setTitle : String -> DropDown msg -> DropDown msg
setTitle title model =
    { model | title = title }


setOnChange : Maybe (String -> msg) -> DropDown msg -> DropDown msg
setOnChange onChange model =
    { model | onChange = onChange }


addOption : ( String, String ) -> DropDown msg -> DropDown msg
addOption option model =
    { model | options = option :: model.options }


setOptions : List ( String, String ) -> DropDown msg -> DropDown msg
setOptions option model =
    { model | options = option }


menuSelectedDecoder : Json.Decoder String
menuSelectedDecoder =
    Json.at [ "detail", "relatedTarget", "dataset", "selectValue" ] Json.string


view : DropDown msg -> Html msg
view { onChange, options, title, defaultValue } =
    H.node "details"
        [ A.class "ui-dropdown" ]
        [ H.node "summary"
            [ A.class "ui-dropdown__summary" ]
            [ Ui.LabelItem.view title
                [ H.p [ A.attribute "data-menu-button" "" ] [ H.text defaultValue ]
                ]
            ]
        , H.node "details-menu"
            [ A.attribute "role" "menu"
            , A.class "ui-dropdown__content"
            , Html.Attributes.Extra.attributeMaybe
                (\action -> E.on "details-menu-selected" (Json.map action menuSelectedDecoder))
                onChange
            ]
            (options
                |> List.map
                    (\( val, text ) ->
                        H.label
                            [ A.tabindex 0
                            , A.attribute "role" "menuitemradio"
                            , A.attribute "data-menu-button-text" ""
                            , A.attribute "data-select-value" val
                            , A.class "ui-dropdown__item"
                            ]
                            [ H.input [ A.type_ "radio", A.name "robot", A.value val ] []
                            , H.text text
                            ]
                    )
            )
        ]
