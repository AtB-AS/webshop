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
    , setCCGLoading
    , setCCGOnCancel
    , setDGLoading
    , setDGMessage
    , setDGOnCancel
    , setDGOnDestroy
    , setEditButtonType
    , setIcon
    , setInEditMode
    , setMessage
    , setOnEdit
    , setOnSave
    )

import Fragment.Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Attributes.Extra
import Html.Events as E
import Html.Extra
import Ui.Button as B exposing (ButtonMode(..))
import Ui.Message exposing (UserStatus)
import Ui.TextContainer exposing (TextColor(..), TextContainer(..))


type alias EditSection msg =
    { accessibilityName : String
    , editButtonData : ( String, Html msg )
    , onEdit : Maybe msg
    , onSave : Maybe msg
    , inEditMode : Bool
    , buttonGroup : Maybe (List (Html msg))
    , icon : Maybe (Html msg)
    , message : Maybe (UserStatus msg)
    }


init : String -> EditSection msg
init accessibilityName =
    { accessibilityName = accessibilityName
    , editButtonData = ( "Endre", Fragment.Icon.edit )
    , onEdit = Nothing
    , onSave = Nothing
    , inEditMode = False
    , buttonGroup = Nothing
    , icon = Nothing
    , message = Nothing
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


setIcon : Maybe (Html msg) -> EditSection msg -> EditSection msg
setIcon icon opts =
    { opts | icon = icon }


setMessage : Maybe (UserStatus msg) -> EditSection msg -> EditSection msg
setMessage message opts =
    { opts | message = message }


editSection : (Bool -> List (Html msg)) -> EditSection msg -> Html msg
editSection children { accessibilityName, editButtonData, onEdit, inEditMode, buttonGroup, onSave, icon, message } =
    let
        ( editText, editIcon ) =
            editButtonData

        iconElement =
            Html.Extra.viewMaybe (List.singleton >> H.div [ A.class "ui-editSection__icon" ]) icon
    in
        H.form
            [ A.class "ui-editSection"
            , A.method "post"
            , Html.Attributes.Extra.attributeMaybe (\action -> E.onSubmit action) onSave
            ]
            [ if not inEditMode then
                H.div [ A.class "ui-editSection__container" ]
                    [ iconElement
                    , H.div [ A.class "ui-editSection__content" ] (children False)
                    , B.init editText
                        |> B.setIcon (Just editIcon)
                        |> B.setOnClick onEdit
                        |> B.setAttributes [ A.class "ui-editSection__editButton" ]
                        |> B.tertiaryCompact
                    ]

              else
                H.fieldset [ A.class "ui-editSection__fieldset" ]
                    [ H.legend
                        [ A.class "ui-editSection__fieldset__legend" ]
                        [ H.text accessibilityName ]
                    , H.div [ A.class "ui-editSection__container" ]
                        [ iconElement
                        , H.div [ A.class "ui-editSection__content" ]
                            [ H.div [] (children True)
                            , case message of
                                Just messageContent ->
                                    H.div [] [ Ui.Message.message messageContent ]

                                Nothing ->
                                    Html.Extra.nothing
                            ]
                        ]
                    , H.div [ A.class "ui-editSection__fieldset__buttonGroup" ] <|
                        Maybe.withDefault [] buttonGroup
                    ]
            ]


type alias CancelConfirmGroup msg =
    { onCancel : Maybe msg
    , loading : Bool
    }


setCCGOnCancel : Maybe msg -> CancelConfirmGroup msg -> CancelConfirmGroup msg
setCCGOnCancel onCancel opts =
    { opts | onCancel = onCancel }


setCCGLoading : Bool -> CancelConfirmGroup msg -> CancelConfirmGroup msg
setCCGLoading loading opts =
    { opts | loading = loading }


cancelConfirmGroup : CancelConfirmGroup msg -> List (Html msg)
cancelConfirmGroup { onCancel, loading } =
    [ B.init
        "Avbryt"
        |> B.setIcon (Just Fragment.Icon.cross)
        |> B.setOnClick onCancel
        |> B.setDisabled loading
        |> B.tertiaryCompact
    , B.init "Lagre"
        |> B.setIcon (Just Fragment.Icon.checkmark)
        |> B.setType "submit"
        |> B.setLoading loading
        |> B.setAttributes [ A.classList [ ( "ui-editSection__fieldset__saveButton", True ) ] ]
        |> B.primaryCompact B.Primary_2
    ]


type alias DestructiveGroup msg =
    { message : String
    , confirmLabel : String
    , onCancel : Maybe msg
    , onDestroy : Maybe msg
    , loading : Bool
    }


initDestructiveGroup : DestructiveGroup msg
initDestructiveGroup =
    { message = ""
    , confirmLabel = ""
    , onCancel = Nothing
    , onDestroy = Nothing
    , loading = False
    }


setDGMessage : String -> DestructiveGroup msg -> DestructiveGroup msg
setDGMessage message opts =
    { opts | message = message }


setDGConfirmLabel : String -> DestructiveGroup msg -> DestructiveGroup msg
setDGConfirmLabel confirmLabel opts =
    { opts | confirmLabel = confirmLabel }


setDGOnCancel : Maybe msg -> DestructiveGroup msg -> DestructiveGroup msg
setDGOnCancel onCancel opts =
    { opts | onCancel = onCancel }


setDGOnDestroy : Maybe msg -> DestructiveGroup msg -> DestructiveGroup msg
setDGOnDestroy onDestroy opts =
    { opts | onDestroy = onDestroy }


setDGLoading : Bool -> DestructiveGroup msg -> DestructiveGroup msg
setDGLoading loading opts =
    { opts | loading = loading }


destructiveGroup : DestructiveGroup msg -> List (Html msg)
destructiveGroup { message, confirmLabel, onCancel, onDestroy, loading } =
    [ H.div [ A.class "ui-editSection__fieldset__buttonGroup__deleteText", A.attribute "role" "alert" ]
        [ H.text message ]
    , B.init
        "Avbryt"
        |> B.setIcon (Just Fragment.Icon.cross)
        |> B.setOnClick onCancel
        |> B.setDisabled loading
        |> B.tertiaryCompact
    , B.init confirmLabel
        |> B.setIcon (Just Fragment.Icon.checkmark)
        |> B.setOnClick onDestroy
        |> B.setLoading loading
        |> B.setAttributes [ A.classList [ ( "ui-editSection__fieldset__saveButton", True ) ] ]
        |> B.primaryCompact B.Primary_destructive
    ]


horizontalGroup : List (Html msg) -> List (Html msg)
horizontalGroup children =
    [ H.div [ A.class "ui-editSection__horizontalGroup" ]
        children
    ]
