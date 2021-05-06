module Ui.TicketDetails exposing (view, viewItem)

import Data.FareContract exposing (FareContract, TravelRight(..))
import Fragment.Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Attributes.Autocomplete exposing (DetailedCompletion(..))
import Html.Attributes.Extra as Attr
import Html.Events as E
import Time
import Ui.TextContainer
import Util.Format


type alias TicketDetails msg =
    { fareContract : FareContract
    , open : Bool
    , onOpenClick : Maybe msg
    , currentTime : Time.Posix
    }


view : TicketDetails msg -> List (Html msg) -> Html msg
view { fareContract, open, onOpenClick, currentTime } children =
    let
        id =
            fareContract.orderId

        classList =
            [ ( "ui-ticketDetails", True )
            , ( "ui-ticketDetails--open", open )
            ]

        classListContent =
            [ ( "ui-ticketDetails__content", True )
            , ( "ui-ticketDetails__content--open", open )
            ]

        classListMetadata =
            [ ( "ui-ticketDetails__metaDataitem", True )
            , ( "ui-ticketDetails__metaDataitem--open", open )
            ]

        classListButton =
            [ ( "ui-ticketDetails__headerButton", True )
            ]

        chevronIcon =
            if open then
                Fragment.Icon.upArrow

            else
                Fragment.Icon.downArrow

        regionId =
            id ++ "region"

        now =
            Time.posixToMillis currentTime

        isCurrentlyActive =
            fareContract.validFrom > now

        icon =
            if isCurrentlyActive then
                Fragment.Icon.ticketLargeValid

            else
                Fragment.Icon.ticketLargeWaiting
    in
        Ui.TextContainer.primary
            [ H.section
                [ A.classList classList ]
                [ H.h2 [ A.class "ui-ticketDetails__header" ]
                    [ H.button
                        [ A.classList classListButton
                        , A.attribute "aria-expanded" (boolAsString open)
                        , A.attribute "aria-controls" regionId
                        , A.id id
                        , Attr.attributeMaybe (\action -> E.onClick action) onOpenClick
                        ]
                        [ H.div [ A.class "ui-ticketDetails__headerButton__icon" ]
                            [ icon ]
                        , H.div [ A.class "ui-ticketDetails__headerButton__title" ]
                            [ viewValidity fareContract.validFrom fareContract.validTo currentTime ]
                        , H.div [ A.class "ui-ticketDetails__headerButton__toggleText" ]
                            [ H.text <|
                                if open then
                                    "Skjul detaljer"

                                else
                                    "Vis detaljer"
                            ]
                        , chevronIcon
                        ]
                    ]
                , H.div [ A.classList classListMetadata ]
                    [ H.p [] [ H.text "dsajdklsajkdjshakj" ] ]
                , H.div
                    [ A.classList classListContent
                    , A.attribute "aria-labelledby" id
                    , Attr.role "region"
                    , A.id regionId
                    , Attr.attributeIf (not open) (A.attribute "inert" "true")
                    ]
                    [ viewItem
                        []
                    ]
                ]
            ]


viewItem : List (Html msg) -> Html msg
viewItem =
    H.div [ A.class "ui-ticketDetails__item" ]


boolAsString : Bool -> String
boolAsString b =
    if b then
        "true"

    else
        "false"


viewValidity : Int -> Int -> Time.Posix -> Html msg
viewValidity from to posixNow =
    let
        now =
            Time.posixToMillis posixNow
    in
        if from > now then
            H.text <| "Gyldig fra " ++ Util.Format.posixToFullHumanized Time.utc (Time.millisToPosix from)

        else if now > to then
            H.text <| timeAgoFormat <| (now - to) // 1000

        else
            H.text <| timeLeftFormat <| (to - now) // 1000


timeToFormat : Int -> String
timeToFormat time =
    "Gyldig  " ++ timeFormat time "" "få strakser"


timeLeftFormat : Int -> String
timeLeftFormat time =
    timeFormat time "igjen" "utløper straks"


timeAgoFormat : Int -> String
timeAgoFormat time =
    timeFormat time "siden" "nå nettopp"


timeFormat : Int -> String -> String -> String
timeFormat time suffix fallback =
    if time >= 86400 then
        pluralFormat (time // 86400) "dag" "dager" ++ " " ++ suffix

    else if time >= 7200 then
        pluralFormat (time // 3600) "time" "timer" ++ " " ++ suffix

    else if time >= 120 then
        pluralFormat (time // 60) "minutt" "minutter" ++ " " ++ suffix

    else if time >= 1 then
        pluralFormat time "sekund" "sekunder" ++ " " ++ suffix

    else
        fallback


pluralFormat : Int -> String -> String -> String
pluralFormat value singular plural =
    String.fromInt value
        ++ " "
        ++ (if value == 1 then
                singular

            else
                plural
           )
