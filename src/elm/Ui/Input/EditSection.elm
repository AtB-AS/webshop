module Ui.Input.EditSection exposing
    ( EditSectionOptions
    , editSection
    , horizontalGroup
    , init
    , setAccessibilityName
    , setEditText
    , setInEditMode
    , setOnCancel
    , setOnEdit
    , setOnSave
    )

import Fragment.Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Attributes.Extra
import Html.Events as E
import Ui.Button as B exposing (ButtonMode(..))
import Ui.TextContainer exposing (TextColor(..), TextContainer(..))


type alias EditSectionOptions msg =
    { accessibilityName : String
    , editText : String
    , onEdit : Maybe msg
    , onSave : Maybe msg
    , onCancel : Maybe msg
    , inEditMode : Bool
    }


init : String -> EditSectionOptions msg
init accessibilityName =
    { accessibilityName = accessibilityName
    , editText = "Endre"
    , onEdit = Nothing
    , onSave = Nothing
    , onCancel = Nothing
    , inEditMode = False
    }


setAccessibilityName : String -> EditSectionOptions msg -> EditSectionOptions msg
setAccessibilityName accessibilityName opts =
    { opts | accessibilityName = accessibilityName }


setEditText : String -> EditSectionOptions msg -> EditSectionOptions msg
setEditText editText opts =
    { opts | editText = editText }


setOnEdit : Maybe msg -> EditSectionOptions msg -> EditSectionOptions msg
setOnEdit onEdit opts =
    { opts | onEdit = onEdit }


setOnSave : Maybe msg -> EditSectionOptions msg -> EditSectionOptions msg
setOnSave onSave opts =
    { opts | onSave = onSave }


setOnCancel : Maybe msg -> EditSectionOptions msg -> EditSectionOptions msg
setOnCancel onCancel opts =
    { opts | onCancel = onCancel }


setInEditMode : Bool -> EditSectionOptions msg -> EditSectionOptions msg
setInEditMode inEditMode opts =
    { opts | inEditMode = inEditMode }


editSection : (Bool -> List (Html msg)) -> EditSectionOptions msg -> Html msg
editSection children { accessibilityName, editText, onEdit, onSave, onCancel, inEditMode } =
    H.form [ A.class "ui-editSection", A.method "post", Html.Attributes.Extra.attributeMaybe (\action -> E.onSubmit action) onSave ]
        [ if not inEditMode then
            H.div [ A.class "ui-editSection__container" ]
                [ H.div [ A.class "ui-editSection__content" ] (children False)
                , B.init editText
                    |> B.setIcon (Just Fragment.Icon.edit)
                    |> B.setOnClick onEdit
                    |> B.setAttributes [ A.class "ui-editSection__editButton" ]
                    |> B.tertiaryCompact
                ]

          else
            H.fieldset [ A.class "ui-editSection__fieldset" ]
                (H.legend
                    [ A.class "ui-editSection__fieldset__legend" ]
                    [ H.text accessibilityName ]
                    :: children True
                    ++ [ H.div [ A.class "ui-editSection__fieldset__buttonGroup" ]
                            [ H.div []
                                []
                            , B.init
                                "Avbryt"
                                |> B.setIcon (Just Fragment.Icon.cross)
                                |> B.setOnClick onCancel
                                |> B.tertiaryCompact
                            , B.init "Lagre"
                                |> B.setIcon (Just Fragment.Icon.checkmark)
                                |> B.setOnClick onSave
                                |> B.setType "submit"
                                |> B.setAttributes [ A.classList [ ( "ui-editSection__fieldset__saveButton", True ) ] ]
                                |> B.primaryCompact B.Primary_2
                            ]
                       ]
                )
        ]


horizontalGroup : List (Html msg) -> List (Html msg)
horizontalGroup children =
    children |> H.div [ A.class "ui-editSection__horizontalGroup" ] |> List.singleton
