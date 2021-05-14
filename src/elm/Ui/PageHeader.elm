module Ui.PageHeader exposing
    ( init
    , setBackButton
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
import Util.Encode exposing (maybe)


type alias PageHeader msg =
    { back : Maybe ( String, Attribute msg, List (Attribute msg) -> List (Html msg) -> Html msg )
    , title : Maybe String
    , onCancel : Maybe ( String, Html msg, msg )
    }


init : PageHeader msg
init =
    { back = Nothing
    , title = Nothing
    , onCancel = Nothing
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


setOnCancel : Maybe ( String, Html msg, msg ) -> PageHeader msg -> PageHeader msg
setOnCancel onCancel opts =
    { opts | onCancel = onCancel }


view : PageHeader msg -> Html msg
view { back, title, onCancel } =
    H.div [ A.class "ui-pageHeader" ]
        [ case back of
            Just ( backTitle, action, el ) ->
                el [ action, A.class "ui-pageHeader__back" ] [ Icon.leftArrow, H.text backTitle ]

            Nothing ->
                Html.Extra.nothing
        , Html.Extra.viewMaybe (Ui.Heading.titleWithEl H.h2) title
        , case onCancel of
            Just ( text, icon, action ) ->
                H.div
                    [ E.onClick action
                    , A.class "ui-pageHeader__cancel"
                    ]
                    [ H.text text, icon ]

            Nothing ->
                Html.Extra.nothing
        ]
