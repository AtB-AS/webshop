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
    H.form [ A.method "post", Html.Attributes.Extra.attributeMaybe (\action -> E.onSubmit action) onSave ]
        [ if not inEditMode then
            H.div []
                (children False
                    ++ [ B.init editText
                            |> B.setIcon (Just Fragment.Icon.edit)
                            |> B.setOnClick onEdit
                            |> B.tertiaryCompact
                       ]
                )

          else
            H.fieldset []
                (H.legend
                    []
                    [ H.text accessibilityName ]
                    :: children True
                    ++ [ H.div []
                            [ B.init "Avbryt"
                                |> B.setIcon (Just Fragment.Icon.cross)
                                |> B.setOnClick onCancel
                                |> B.tertiaryCompact
                            , B.init "Lagre"
                                |> B.setIcon (Just Fragment.Icon.checkmark)
                                |> B.setOnClick onSave
                                |> B.setType "submit"
                                |> B.primaryCompact B.Secondary_2
                            ]
                       ]
                )
        ]
