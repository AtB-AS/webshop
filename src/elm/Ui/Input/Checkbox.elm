module Ui.Input.Checkbox exposing
    ( Checkbox
    , init
    , setAttributes
    , setChecked
    , setId
    , setName
    , setOnCheck
    , setTitle
    , view
    )

import Fragment.Icon as Icon
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Events as E


type alias Checkbox msg =
    { id : String
    , name : String
    , title : String
    , onCheck : Maybe (Bool -> msg)
    , checked : Bool
    , attributes : List (Attribute msg)
    }


init : String -> Checkbox msg
init id =
    { id = id
    , name = ""
    , title = ""
    , checked = False
    , onCheck = Nothing
    , attributes = []
    }


setId : String -> Checkbox msg -> Checkbox msg
setId id opts =
    { opts | id = id }


setName : String -> Checkbox msg -> Checkbox msg
setName name opts =
    { opts | name = name }


setTitle : String -> Checkbox msg -> Checkbox msg
setTitle title opts =
    { opts | title = title }


setChecked : Bool -> Checkbox msg -> Checkbox msg
setChecked checked opts =
    { opts | checked = checked }


setOnCheck : Maybe (Bool -> msg) -> Checkbox msg -> Checkbox msg
setOnCheck onCheck opts =
    { opts | onCheck = onCheck }


setAttributes : List (Attribute msg) -> Checkbox msg -> Checkbox msg
setAttributes attributes opts =
    { opts | attributes = attributes }


view : Checkbox msg -> Html msg
view { id, name, title, onCheck, checked, attributes } =
    H.div []
        [ H.input
            ([ A.type_ "checkbox"
             , A.id id
             , A.name name
             , A.class "ui-input-checkbox__input"
             , A.checked checked
             ]
                ++ (onCheck |> Maybe.map (E.onCheck >> List.singleton) |> Maybe.withDefault [])
                ++ attributes
            )
            []
        , H.label [ A.for id, A.class "ui-input-checkbox" ]
            [ H.div [ A.class "ui-input-checkbox__box" ]
                [ if checked then
                    Icon.checkOn

                  else
                    Icon.checkOff
                ]
            , H.div [ A.class "ui-input-checkbox__title" ] [ H.text title ]
            ]
        ]
