module Ui.Message exposing
    ( Border(..)
    , Message
    , UserStatus(..)
    , defaultOption
    , error
    , errorWithOptions
    , info
    , infoWithOptions
    , message
    , messageWithOptions
    , valid
    , validWithOptions
    , warning
    , warningWithOptions
    )

import Fragment.Icon as Icon
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Attributes.Extra as Attr


type UserStatus msg
    = Warning (Html msg)
    | Info (Html msg)
    | Valid (Html msg)
    | Error (Html msg)


type Border
    = Top
    | Bottom
    | TopBottom
    | NoBorder


statusToClass : UserStatus msg -> String
statusToClass status =
    case status of
        Warning _ ->
            "ui-message--warning"

        Error _ ->
            "ui-message--error"

        Valid _ ->
            "ui-message--valid"

        Info _ ->
            "ui-message--info"


statusToIcon : UserStatus msg -> Html msg
statusToIcon status =
    case status of
        Warning _ ->
            Icon.warning

        Error _ ->
            Icon.error

        Valid _ ->
            Icon.checkmarkCircle

        Info _ ->
            Icon.info


statusToAttribute : UserStatus msg -> Maybe (Attribute msg)
statusToAttribute status =
    case status of
        Warning _ ->
            Just <| A.attribute "aria-live" "polite"

        Valid _ ->
            Just <| A.attribute "aria-live" "polite"

        Error _ ->
            Just <| A.attribute "role" "alert"

        Info _ ->
            Nothing


stringOfStatus : UserStatus msg -> Html msg
stringOfStatus status =
    case status of
        Warning text ->
            text

        Error text ->
            text

        Valid text ->
            text

        Info text ->
            text


type alias Message =
    { borderTop : Bool
    , borderBottom : Bool
    , marginTop : Bool
    , marginBottom : Bool
    }


messageWithOptions : Message -> UserStatus msg -> Html msg
messageWithOptions options statusType =
    let
        statusClass =
            statusToClass statusType

        classList =
            [ ( "ui-message", True )
            , ( statusClass, True )
            , ( "ui-message--borderTop", options.borderTop )
            , ( "ui-message--borderBottom", options.borderBottom )
            , ( "ui-message--marginTop", options.marginTop )
            , ( "ui-message--marginBottom", options.marginBottom )
            ]

        text =
            stringOfStatus statusType

        icon =
            statusToIcon statusType
    in
        H.div [ A.classList classList ]
            [ icon
            , H.div
                [ A.class "ui-message__content"
                , Attr.attributeMaybe identity (statusToAttribute statusType)
                ]
                [ text ]
            ]


defaultOption : Message
defaultOption =
    { borderTop = False
    , borderBottom = False
    , marginTop = False
    , marginBottom = False
    }


message : UserStatus msg -> Html msg
message =
    messageWithOptions defaultOption


infoWithOptions : Message -> String -> Html msg
infoWithOptions opts text =
    messageWithOptions opts (Info <| H.text text)


warningWithOptions : Message -> String -> Html msg
warningWithOptions opts text =
    messageWithOptions opts (Warning <| H.text text)


validWithOptions : Message -> String -> Html msg
validWithOptions opts text =
    messageWithOptions opts (Valid <| H.text text)


errorWithOptions : Message -> String -> Html msg
errorWithOptions opts text =
    messageWithOptions opts (Error <| H.text text)


info : String -> Html msg
info =
    infoWithOptions defaultOption


warning : String -> Html msg
warning =
    warningWithOptions defaultOption


valid : String -> Html msg
valid =
    validWithOptions defaultOption


error : String -> Html msg
error =
    errorWithOptions defaultOption
