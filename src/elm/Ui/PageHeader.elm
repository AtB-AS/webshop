module Ui.PageHeader exposing
    ( init
    , setBackButton
    , setBackIcon
    , setBackRoute
    , setTitle
    , view
    )

import Fragment.Icon as Icon
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Extra
import Route exposing (Route)
import Ui.TextContainer


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


view : PageHeader msg -> Html msg
view { back, title, backIcon } =
    H.div [ A.class "ui-pageHeader" ]
        [ case back of
            Just ( backTitle, action, el ) ->
                el [ action, A.class "ui-pageHeader__back" ] [ backIcon, H.text backTitle ]

            Nothing ->
                Html.Extra.nothing
        , Html.Extra.viewMaybe (\text -> H.h2 [ A.class "ui-pageHeader__title" ] [ Ui.TextContainer.primaryJumboInline [ H.text text ] ]) title
        ]
