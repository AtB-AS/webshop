module Ui.Input.EditSection exposing
    ( EditSectionOptions
    , editSection
    )

import Fragment.Icon
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Attributes.Extra
import Html.Events as E
import Html.Extra
import Ui.Button as B exposing (ButtonMode(..))
import Ui.Group
import Ui.TextContainer as Text exposing (TextColor(..), TextContainer(..))


type alias EditSectionOptions msg =
    { accessibilityName : String
    , editText : String
    , onEdit : Maybe msg
    , onSave : Maybe msg
    , onCancel : Maybe msg
    , inEditMode : Bool
    }


editSection : EditSectionOptions msg -> (Bool -> List (Html msg)) -> Html msg
editSection { accessibilityName, editText, onEdit, onSave, onCancel, inEditMode } children =
    H.form [ A.class "ui-editSection", A.method "post", Html.Attributes.Extra.attributeMaybe (\action -> E.onSubmit action) onSave ]
        [ if not inEditMode then
            H.div [ A.class "ui-editSection__container" ]
                [ H.div [ A.class "ui-editSection__content" ] (children False)
                , B.init editText
                    |> B.setIcon (Just Fragment.Icon.edit)
                    |> B.setOnClick onEdit
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
