module Ui.TicketDetails exposing (view, viewItem)

import Data.FareContract exposing (FareContract, TravelRight(..), TravelRightFull)
import Data.RefData exposing (LangString(..))
import Dict exposing (Dict)
import Fragment.Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Attributes.Autocomplete exposing (DetailedCompletion(..))
import Html.Attributes.Extra as Attr
import Html.Events as E
import Html.Extra
import List.Extra
import Shared exposing (Shared)
import Time
import Ui.LabelItem
import Ui.TextContainer
import Util.Format


type alias TicketDetails msg =
    { fareContract : FareContract
    , open : Bool
    , onOpenClick : Maybe msg
    , currentTime : Time.Posix
    }


view : Shared -> TicketDetails msg -> Html msg
view shared { fareContract, open, onOpenClick, currentTime } =
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
                    (List.map
                        (viewTravelRight shared)
                        fareContract.travelRights
                    )
                , H.div
                    [ A.classList classListContent
                    , A.attribute "aria-labelledby" id
                    , Attr.role "region"
                    , A.id regionId
                    , Attr.attributeIf (not open) (A.attribute "inert" "true")
                    ]
                    [ viewHorizontalItem
                        [ viewLabelTime "Gyldig fra" fareContract.validFrom
                        , viewLabelTime "Gyldig til" fareContract.validTo
                        ]
                    , viewHorizontalItem
                        [ Ui.LabelItem.viewCompact "Kjøpstidspunkt" [ H.text <| Util.Format.dateTime fareContract.created ]
                        , Ui.LabelItem.viewCompact "Betalt med" [ H.text <| formatPaymentType fareContract.paymentType ]
                        , Ui.LabelItem.viewCompact "Ordre-ID" [ H.text fareContract.orderId ]
                        ]
                    ]
                ]
            ]


viewTravelRight : Shared -> TravelRight -> Html msg
viewTravelRight shared travelRight =
    case travelRight of
        UnknownTicket _ ->
            Html.Extra.nothing

        SingleTicket ticket ->
            viewTravelRightFull shared ticket

        PeriodTicket ticket ->
            viewTravelRightFull shared ticket


viewTravelRightFull : Shared -> TravelRightFull -> Html msg
viewTravelRightFull shared travelRight =
    let
        product =
            shared.fareProducts
                |> List.Extra.find
                    (\entry ->
                        entry.id == travelRight.fareProductRef
                    )

        zones =
            travelRight.tariffZoneRefs
                |> frequency
                |> Dict.map
                    (\ref _ ->
                        shared.tariffZones
                            |> List.filter (.id >> (==) ref)
                            |> List.head
                            |> Maybe.map (\zone -> langString zone.name)
                            |> Maybe.withDefault ""
                    )
                |> Dict.values
                |> List.filter ((/=) "")

        numZones =
            List.length zones

        firstZone =
            List.head (Debug.log "zoneNames" zones)

        lastZone =
            List.Extra.last zones

        zoneString =
            if firstZone == lastZone || lastZone == Nothing then
                "Reise i 1 sone (Sone " ++ Maybe.withDefault "" firstZone ++ ")"

            else
                "Reise i " ++ pluralFormat numZones "sone" "soner" ++ " (Sone " ++ Maybe.withDefault "" firstZone ++ " til " ++ Maybe.withDefault "" lastZone ++ ")"
    in
        case product of
            Nothing ->
                Html.Extra.nothing

            Just p ->
                viewHorizontalItem
                    [ H.div []
                        [ H.p [] [ H.text <| langString p.name ]
                        , H.p [] [ H.text zoneString ]
                        ]
                    , H.div [] [ H.text "dsa" ]
                    ]


viewLabelTime : String -> Int -> Html msg
viewLabelTime title dateTime =
    Ui.LabelItem.viewCompact title
        [ dateTime
            |> Time.millisToPosix
            |> Util.Format.posixToFullHumanized Time.utc
            |> H.text
        ]


viewItem : List (Html msg) -> Html msg
viewItem =
    H.div [ A.class "ui-ticketDetails__item" ]


viewHorizontalItem : List (Html msg) -> Html msg
viewHorizontalItem =
    H.div [ A.class "ui-ticketDetails__item ui-ticketDetails__item--horizontal" ]


formatPaymentType : List String -> String
formatPaymentType types =
    case types of
        [] ->
            "??"

        [ "VIPPS" ] ->
            "Vipps"

        [ "VISA" ] ->
            "Bankkort"

        _ ->
            "??"


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


frequency : List comparable -> Dict comparable Int
frequency =
    List.foldl
        (\item ->
            Dict.update item (Maybe.map ((+) 1) >> Maybe.withDefault 1 >> Just)
        )
        Dict.empty


langString : LangString -> String
langString (LangString _ value) =
    value
