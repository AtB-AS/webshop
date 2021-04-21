module Ui.Input.EditSection exposing
    ( EditSectionOptions
    , cancelConfirmGroup
    , destructiveGroup
    , editSection
    , horizontalGroup
    , init
    , setAccessibilityName
    , setButtonGroup
    , setEditButtonType
    , setInEditMode
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
    , editButtonData : ( String, Html msg )
    , onEdit : Maybe msg
    , onSave : Maybe msg
    , inEditMode : Bool
    , buttonGroup : Maybe (List (Html msg))
    }


init : String -> EditSectionOptions msg
init accessibilityName =
    { accessibilityName = accessibilityName
    , editButtonData = ( "Endre", Fragment.Icon.edit )
    , onEdit = Nothing
    , onSave = Nothing
    , inEditMode = False
    , buttonGroup = Nothing
    }


setAccessibilityName : String -> EditSectionOptions msg -> EditSectionOptions msg
setAccessibilityName accessibilityName opts =
    { opts | accessibilityName = accessibilityName }


setEditButtonType : ( String, Html msg ) -> EditSectionOptions msg -> EditSectionOptions msg
setEditButtonType editButtonData opts =
    { opts | editButtonData = editButtonData }


setOnEdit : Maybe msg -> EditSectionOptions msg -> EditSectionOptions msg
setOnEdit onEdit opts =
    { opts | onEdit = onEdit }


setOnSave : Maybe msg -> EditSectionOptions msg -> EditSectionOptions msg
setOnSave onSave opts =
    { opts | onSave = onSave }


setInEditMode : Bool -> EditSectionOptions msg -> EditSectionOptions msg
setInEditMode inEditMode opts =
    { opts | inEditMode = inEditMode }


setButtonGroup : Maybe (List (Html msg)) -> EditSectionOptions msg -> EditSectionOptions msg
setButtonGroup buttonGroup opts =
    { opts | buttonGroup = buttonGroup }


editSection : (Bool -> List (Html msg)) -> EditSectionOptions msg -> Html msg
editSection children { accessibilityName, editButtonData, onEdit, inEditMode, buttonGroup, onSave } =
    let
        ( editText, editIcon ) =
            editButtonData
    in
        H.form
            [ A.class "ui-editSection"
            , A.method "post"
            , Html.Attributes.Extra.attributeMaybe (\action -> E.onSubmit action) onSave
            ]
            [ if not inEditMode then
                H.div [ A.class "ui-editSection__container" ]
                    [ H.div [ A.class "ui-editSection__content" ] (children False)
                    , B.init editText
                        |> B.setIcon (Just editIcon)
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
                        ++ [ H.div [ A.class "ui-editSection__fieldset__buttonGroup" ] <|
                                Maybe.withDefault [] buttonGroup
                           ]
                    )
            ]


cancelConfirmGroup : Maybe msg -> List (Html msg)
cancelConfirmGroup onCancel =
    [ B.init
        "Avbryt"
        |> B.setIcon (Just Fragment.Icon.cross)
        |> B.setOnClick onCancel
        |> B.tertiaryCompact
    , B.init "Lagre"
        |> B.setIcon (Just Fragment.Icon.checkmark)
        |> B.setType "submit"
        |> B.setAttributes [ A.classList [ ( "ui-editSection__fieldset__saveButton", True ) ] ]
        |> B.primaryCompact B.Primary_2
    ]


destructiveGroup : String -> Maybe msg -> Maybe msg -> List (Html msg)
destructiveGroup message onCancel onDestroy =
    [ H.div [ A.class "ui-editSection__fieldset__buttonGroup__deleteText" ]
        [ H.text message ]
    , B.init
        "Avbryt"
        |> B.setIcon (Just Fragment.Icon.cross)
        |> B.setOnClick onCancel
        |> B.tertiaryCompact
    , B.init "Fjern t:kort"
        |> B.setIcon (Just Fragment.Icon.checkmark)
        |> B.setOnClick onDestroy
        |> B.setAttributes [ A.classList [ ( "ui-editSection__fieldset__saveButton", True ) ] ]
        |> B.primaryCompact B.Primary_destructive
    ]


horizontalGroup : List (Html msg) -> List (Html msg)
horizontalGroup children =
    children |> H.div [ A.class "ui-editSection__horizontalGroup" ] |> List.singleton
