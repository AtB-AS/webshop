module Ui.PageHeader exposing
    ( init
    , setBackButton
    , setBackRoute
    , setTitle
    , view
    )

import Fragment.Icon as Icon
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Extra
import Route exposing (Route)
import Ui.Heading


type alias PageHeader msg =
    { back : Maybe ( Attribute msg, String, List (Attribute msg) -> List (Html msg) -> Html msg )
    , title : Maybe String
    }


init : PageHeader msg
init =
    { back = Nothing
    , title = Nothing
    }


setBackButton : ( Attribute msg, String ) -> PageHeader msg -> PageHeader msg
setBackButton ( action, text ) opts =
    { opts | back = Just ( action, text, H.button ) }


setBackRoute : ( Route, String ) -> PageHeader msg -> PageHeader msg
setBackRoute ( route, backText ) opts =
    { opts | back = Just ( Route.href route, backText, H.a ) }


setTitle : Maybe String -> PageHeader msg -> PageHeader msg
setTitle title opts =
    { opts | title = title }


view : PageHeader msg -> Html msg
view { back, title } =
    H.div [ A.class "ui-pageHeader" ]
        [ case back of
            Just ( action, backTitle, el ) ->
                el [ action, A.class "ui-pageHeader__back" ] [ Icon.leftArrow, H.text backTitle ]

            Nothing ->
                Html.Extra.nothing
        , Html.Extra.viewMaybe (Ui.Heading.titleWithEl H.h2) title
        ]
