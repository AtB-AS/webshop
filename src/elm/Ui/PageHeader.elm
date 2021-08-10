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
import Html.Events as E
import Html.Extra
import Html.Keyed as Keyed
import Route exposing (Route)
import Ui.Button as B
import Ui.TextContainer


type alias PageHeader msg =
    { back : Maybe ( String, Attribute msg, List (Attribute msg) -> List (Html msg) -> Html msg )
    , title : Maybe String
    , backIcon : Html msg
    }


init : PageHeader msg
init =
    { back = Nothing
    , title = Nothing
    , backIcon = Icon.leftArrow
    }


setBackButton : ( String, msg ) -> PageHeader msg -> PageHeader msg
setBackButton ( backText, action ) opts =
    { opts | back = Just ( backText, E.onClick action, H.button ) }


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
        [ Html.Extra.viewMaybe (\text -> Keyed.node "h2" [ A.class "ui-pageHeader__title" ] [ ( text, Ui.TextContainer.primaryJumboInline [ H.text text ] ) ]) title
        , viewMaybeButton back backIcon
        ]


viewMaybeButton : Maybe ( String, Attribute msg, List (Attribute msg) -> List (Html msg) -> Html msg ) -> Html msg -> Html msg
viewMaybeButton maybeButton backIcon =
    case maybeButton of
        Just ( title, action, el ) ->
            B.init title
                |> B.setIcon (Just backIcon)
                |> B.setIconPosition B.Left
                |> B.setElement el
                |> B.setTransparent True
                |> B.setAttributes [ action, A.class "ui-pageHeader__back" ]
                |> B.tertiaryCompact

        _ ->
            Html.Extra.nothing
