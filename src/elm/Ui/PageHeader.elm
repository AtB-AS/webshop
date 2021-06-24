module Ui.PageHeader exposing
    ( init
    , setBackButton
    , setBackIcon
    , setBackRoute
    , setOnCancel
    , setTitle
    , view
    )

import Fragment.Icon as Icon
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Events as E
import Html.Extra
import Route exposing (Route)
import Ui.Heading


type alias PageHeader msg =
    { back : Maybe ( String, Attribute msg, List (Attribute msg) -> List (Html msg) -> Html msg )
    , title : Maybe String
    , onCancel : Maybe ( String, Html msg, msg )
    , backIcon : Html msg
    }


init : PageHeader msg
init =
    { back = Nothing
    , title = Nothing
    , onCancel = Nothing
    , backIcon = Icon.leftArrow
    }


setBackButton : Maybe ( String, Attribute msg ) -> PageHeader msg -> PageHeader msg
setBackButton maybeAction opts =
    let
        newBack =
            Maybe.map (\( text, action ) -> ( text, action, H.button )) maybeAction
    in
        { opts | back = newBack }


setBackRoute : ( String, Route ) -> PageHeader msg -> PageHeader msg
setBackRoute ( backText, route ) opts =
    { opts | back = Just ( backText, Route.href route, H.a ) }


setTitle : Maybe String -> PageHeader msg -> PageHeader msg
setTitle title opts =
    { opts | title = title }


setBackIcon : Html msg -> PageHeader msg -> PageHeader msg
setBackIcon backIcon opts =
    { opts | backIcon = backIcon }


setOnCancel : Maybe ( String, Html msg, msg ) -> PageHeader msg -> PageHeader msg
setOnCancel onCancel opts =
    { opts | onCancel = onCancel }


view : PageHeader msg -> Html msg
view { back, title, onCancel, backIcon } =
    H.div [ A.class "ui-pageHeader" ]
        [ case back of
            Just ( backTitle, action, el ) ->
                el [ action, A.class "ui-pageHeader__back" ] [ backIcon, H.text backTitle ]

            Nothing ->
                Html.Extra.nothing
        , Html.Extra.viewMaybe (Ui.Heading.titleWithEl H.h2) title
        , case onCancel of
            Just ( text, icon, action ) ->
                H.button
                    [ E.onClick action
                    , A.class "ui-pageHeader__cancel"
                    , A.type_ "button"
                    ]
                    [ H.text text, icon ]

            Nothing ->
                Html.Extra.nothing
        ]
