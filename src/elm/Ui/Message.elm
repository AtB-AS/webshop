module Ui.Message exposing
    ( Border(..)
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
import Html as H exposing (Html)
import Html.Attributes as A


type UserStatus
    = Warning String
    | Info String
    | Valid String
    | Error String


type Border
    = Top
    | Bottom
    | TopBottom
    | NoBorder


statusToClass : UserStatus -> String
statusToClass status =
    case status of
        Warning _ ->
            "message--warning"

        Error _ ->
            "message--error"

        Valid _ ->
            "message--valid"

        Info _ ->
            "message--info"


statusToIcon : UserStatus -> Html msg
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


stringOfStatus : UserStatus -> String
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


type alias MessageOptions =
    { borderTop : Bool
    , borderBottom : Bool
    , marginTop : Bool
    , marginBottom : Bool
    }


messageWithOptions : MessageOptions -> UserStatus -> Html msg
messageWithOptions options statusType =
    let
        statusClass =
            statusToClass statusType

        classList =
            [ ( "message", True )
            , ( statusClass, True )
            , ( "message--borderTop", options.borderTop )
            , ( "message--borderBottom", options.borderBottom )
            , ( "message--marginTop", options.marginTop )
            , ( "message--marginBottom", options.marginBottom )
            ]

        text =
            H.text <| stringOfStatus statusType

        icon =
            statusToIcon statusType
    in
        H.div [ A.classList classList ]
            [ icon
            , H.div [ A.class "message__content" ] [ text ]
            ]


defaultOption : MessageOptions
defaultOption =
    { borderTop = False
    , borderBottom = False
    , marginTop = False
    , marginBottom = False
    }


message : UserStatus -> Html msg
message =
    messageWithOptions defaultOption


infoWithOptions : MessageOptions -> String -> Html msg
infoWithOptions opts text =
    messageWithOptions opts (Info text)


warningWithOptions : MessageOptions -> String -> Html msg
warningWithOptions opts text =
    messageWithOptions opts (Warning text)


validWithOptions : MessageOptions -> String -> Html msg
validWithOptions opts text =
    messageWithOptions opts (Valid text)


errorWithOptions : MessageOptions -> String -> Html msg
errorWithOptions opts text =
    messageWithOptions opts (Error text)


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
