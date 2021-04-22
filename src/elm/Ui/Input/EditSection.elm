module Ui.Input.EditSection exposing
    ( EditSection
    , cancelConfirmGroup
    , destructiveGroup
    , editSection
    , horizontalGroup
    , init
    , initDestructiveGroup
    , setAccessibilityName
    , setButtonGroup
    , setCCGDisabled
    , setCCGOnCancel
    , setDGDisabled
    , setDGMessage
    , setDGOnCancel
    , setDGOnDestroy
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


type alias EditSection msg =
    { accessibilityName : String
    , editButtonData : ( String, Html msg )
    , onEdit : Maybe msg
    , onSave : Maybe msg
    , inEditMode : Bool
    , buttonGroup : Maybe (List (Html msg))
    }


init : String -> EditSection msg
init accessibilityName =
    { accessibilityName = accessibilityName
    , editButtonData = ( "Endre", Fragment.Icon.edit )
    , onEdit = Nothing
    , onSave = Nothing
    , inEditMode = False
    , buttonGroup = Nothing
    }


setAccessibilityName : String -> EditSection msg -> EditSection msg
setAccessibilityName accessibilityName opts =
    { opts | accessibilityName = accessibilityName }


setEditButtonType : ( String, Html msg ) -> EditSection msg -> EditSection msg
setEditButtonType editButtonData opts =
    { opts | editButtonData = editButtonData }


setOnEdit : Maybe msg -> EditSection msg -> EditSection msg
setOnEdit onEdit opts =
    { opts | onEdit = onEdit }


setOnSave : Maybe msg -> EditSection msg -> EditSection msg
setOnSave onSave opts =
    { opts | onSave = onSave }


setInEditMode : Bool -> EditSection msg -> EditSection msg
setInEditMode inEditMode opts =
    { opts | inEditMode = inEditMode }


setButtonGroup : Maybe (List (Html msg)) -> EditSection msg -> EditSection msg
setButtonGroup buttonGroup opts =
    { opts | buttonGroup = buttonGroup }


editSection : (Bool -> List (Html msg)) -> EditSection msg -> Html msg
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


type alias CancelConfirmGroup msg =
    { onCancel : Maybe msg
    , disabled : Bool
    }


setCCGOnCancel : Maybe msg -> CancelConfirmGroup msg -> CancelConfirmGroup msg
setCCGOnCancel onCancel opts =
    { opts | onCancel = onCancel }


setCCGDisabled : Bool -> CancelConfirmGroup msg -> CancelConfirmGroup msg
setCCGDisabled disabled opts =
    { opts | disabled = disabled }


cancelConfirmGroup : CancelConfirmGroup msg -> List (Html msg)
cancelConfirmGroup { onCancel, disabled } =
    [ B.init
        "Avbryt"
        |> B.setIcon (Just Fragment.Icon.cross)
        |> B.setOnClick onCancel
        |> B.setDisabled disabled
        |> B.tertiaryCompact
    , B.init "Lagre"
        |> B.setIcon (Just Fragment.Icon.checkmark)
        |> B.setType "submit"
        |> B.setDisabled disabled
        |> B.setAttributes [ A.classList [ ( "ui-editSection__fieldset__saveButton", True ) ] ]
        |> B.primaryCompact B.Primary_2
    ]


type alias DestructiveGroup msg =
    { message : String
    , onCancel : Maybe msg
    , onDestroy : Maybe msg
    , disabled : Bool
    }


initDestructiveGroup : DestructiveGroup msg
initDestructiveGroup =
    { message = ""
    , onCancel = Nothing
    , onDestroy = Nothing
    , disabled = False
    }


setDGMessage : String -> DestructiveGroup msg -> DestructiveGroup msg
setDGMessage message opts =
    { opts | message = message }


setDGOnCancel : Maybe msg -> DestructiveGroup msg -> DestructiveGroup msg
setDGOnCancel onCancel opts =
    { opts | onCancel = onCancel }


setDGOnDestroy : Maybe msg -> DestructiveGroup msg -> DestructiveGroup msg
setDGOnDestroy onDestroy opts =
    { opts | onDestroy = onDestroy }


setDGDisabled : Bool -> DestructiveGroup msg -> DestructiveGroup msg
setDGDisabled disabled opts =
    { opts | disabled = disabled }


destructiveGroup : DestructiveGroup msg -> List (Html msg)
destructiveGroup { message, onCancel, onDestroy, disabled } =
    [ H.div [ A.class "ui-editSection__fieldset__buttonGroup__deleteText" ]
        [ H.text message ]
    , B.init
        "Avbryt"
        |> B.setIcon (Just Fragment.Icon.cross)
        |> B.setOnClick onCancel
        |> B.setDisabled disabled
        |> B.tertiaryCompact
    , B.init "Fjern t:kort"
        |> B.setIcon (Just Fragment.Icon.checkmark)
        |> B.setOnClick onDestroy
        |> B.setDisabled disabled
        |> B.setAttributes [ A.classList [ ( "ui-editSection__fieldset__saveButton", True ) ] ]
        |> B.primaryCompact B.Primary_destructive
    ]


horizontalGroup : List (Html msg) -> List (Html msg)
horizontalGroup children =
    children |> H.div [ A.class "ui-editSection__horizontalGroup" ] |> List.singleton
