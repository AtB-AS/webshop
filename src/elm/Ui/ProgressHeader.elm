module Ui.ProgressHeader exposing
    ( init
    , setBack
    , setNext
    , setStep
    , setTitle
    , setTotalSteps
    , view
    )

import Browser.Navigation exposing (back)
import Fragment.Icon as Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Extra
import Ui.Button as B
import Ui.Heading exposing (title)
import Ui.TextContainer


type alias PageHeader msg =
    { back : Maybe ( String, msg )
    , title : String
    , next : Maybe ( String, msg )
    , totalSteps : Int
    , step : Int
    }


init : String -> PageHeader msg
init title =
    { back = Nothing
    , title = title
    , next = Nothing
    , totalSteps = 0
    , step = 0
    }


setBack : Maybe ( String, msg ) -> PageHeader msg -> PageHeader msg
setBack back opts =
    { opts | back = back }


setTitle : String -> PageHeader msg -> PageHeader msg
setTitle title opts =
    { opts | title = title }


setNext : Maybe ( String, msg ) -> PageHeader msg -> PageHeader msg
setNext next opts =
    { opts | next = next }


setStep : Int -> PageHeader msg -> PageHeader msg
setStep step opts =
    { opts | step = step }


setTotalSteps : Int -> PageHeader msg -> PageHeader msg
setTotalSteps totalSteps opts =
    { opts | totalSteps = totalSteps }


view : PageHeader msg -> Html msg
view { title, back, next, step, totalSteps } =
    H.div [ A.class "ui-progressHeader" ]
        [ H.h2 [ A.class "ui-progressHeader__title" ] [ Ui.TextContainer.primaryJumboInline [ H.text title ] ]
        , H.div [ A.class "ui-progressHeader__progressContainer" ]
            [ viewMaybeButton Left back
            , viewProgress title step totalSteps
            , viewMaybeButton Right next
            ]
        ]



--  INTERNALS


viewProgress : String -> Int -> Int -> Html msg
viewProgress title step totalSteps =
    let
        stepTitle =
            "Steg " ++ String.fromInt step ++ " av " ++ String.fromInt totalSteps ++ ": " ++ title
    in
        H.div
            [ A.attribute "role" "progressbar"
            , A.attribute "aria-valuemin" "1"
            , A.attribute "aria-valuemax" <| String.fromInt totalSteps
            , A.attribute "aria-valuenow" <| String.fromInt step
            , A.attribute "aria-valuetext" stepTitle
            , A.class "ui-progressHeader__dots"
            ]
            (totalSteps
                - 1
                |> List.range 0
                |> List.indexedMap (viewDot step)
            )


viewDot : Int -> Int -> Int -> Html msg
viewDot currentStep index _ =
    let
        classList =
            [ ( "ui-progressHeader__dot", True )
            , ( "ui-progressHeader__dot--active", currentStep > index )
            ]
    in
        H.div [ A.classList classList ] []


type Direction
    = Left
    | Right


viewMaybeButton : Direction -> Maybe ( String, msg ) -> Html msg
viewMaybeButton direction maybeButton =
    let
        classList =
            [ ( "ui-progressHeader__button", True )
            , ( "ui-progressHeader__button--left", direction == Left )
            , ( "ui-progressHeader__button--right", direction == Right )
            ]

        icon =
            case direction of
                Left ->
                    Icon.leftArrow

                Right ->
                    Icon.rightArrow
    in
        case maybeButton of
            Just ( title, action ) ->
                B.init title
                    |> B.setOnClick (Just action)
                    |> B.setIcon (Just icon)
                    |> B.setAttributes [ A.classList classList ]
                    |> B.tertiaryCompact

            _ ->
                Html.Extra.nothing
