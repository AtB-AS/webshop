module Ui.Message exposing
    ( Border(..)
    , error
    , errorWithOptions
    , info
    , infoWithOptions
    , valid
    , validWithOptions
    , warning
    , warningWithOptions
    )

import Fragment.Icon as Icon
import Html as H exposing (Html)
import Html.Attributes as A


type UserStatus
    = Warning
    | Info
    | Valid
    | Error


type Border
    = Top
    | Bottom
    | TopBottom
    | NoBorder


statusToClass : UserStatus -> String
statusToClass status =
    case status of
        Warning ->
            "message--warning"

        Error ->
            "message--error"

        Valid ->
            "message--valid"

        Info ->
            "message--info"


statusToIcon : UserStatus -> Html msg
statusToIcon status =
    case status of
        Warning ->
            Icon.warning

        Error ->
            Icon.error

        Valid ->
            Icon.checkmarkCircle

        Info ->
            Icon.info


type alias MessageOptions =
    { borderTop : Bool
    , borderBottom : Bool
    , marginTop : Bool
    , marginBottom : Bool
    }


base : UserStatus -> MessageOptions -> Html msg -> Html msg
base statusType options text =
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


infoWithOptions : MessageOptions -> Html msg -> Html msg
infoWithOptions =
    base Info


warningWithOptions : MessageOptions -> Html msg -> Html msg
warningWithOptions =
    base Warning


validWithOptions : MessageOptions -> Html msg -> Html msg
validWithOptions =
    base Valid


errorWithOptions : MessageOptions -> Html msg -> Html msg
errorWithOptions =
    base Error


info : Html msg -> Html msg
info =
    infoWithOptions defaultOption


warning : Html msg -> Html msg
warning =
    warningWithOptions defaultOption


valid : Html msg -> Html msg
valid =
    validWithOptions defaultOption


error : Html msg -> Html msg
error =
    errorWithOptions defaultOption
