module Ui.TicketDetails exposing (view, viewActivation)

import Data.FareContract exposing (FareContract, TravelRight(..), TravelRightCarnet, TravelRightFull)
import Data.RefData exposing (LangString(..))
import Data.Ticket exposing (Reservation, ReservationStatus(..))
import Dict exposing (Dict)
import Dict.Extra
import Fragment.Icon as Icon
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
import Util.Time as TimeUtil


type alias TicketDetails msg =
    { fareContract : FareContract
    , open : Bool
    , onOpenClick : Maybe msg
    , currentTime : Time.Posix
    , timeZone : Time.Zone
    }


type alias TravelRightSummary =
    { zones : List String
    , fareProductRef : String
    , userProfilesRefs : List ( String, Int )
    }


view : Shared -> TicketDetails msg -> Html msg
view shared ticketDetails =
    let
        { fareContract, open, onOpenClick, timeZone } =
            ticketDetails

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
                Icon.upArrow

            else
                Icon.downArrow

        regionId =
            id ++ "region"
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
                      <|
                        viewTicketButtonTextAndIcon ticketDetails
                            ++ [ H.span [ A.class "ui-ticketDetails__headerButton__toggleText" ]
                                    [ Ui.TextContainer.tertiary
                                        [ H.text <|
                                            if open then
                                                "Skjul detaljer"

                                            else
                                                "Vis detaljer"
                                        , chevronIcon
                                        ]
                                    ]
                               ]
                    ]
                , H.div [ A.classList classListMetadata ]
                    (fareContract.travelRights
                        |> onlyTravelRightFull
                        |> groupTravelRights
                        |> List.map (viewTravelRightSummary shared)
                    )
                , H.div
                    [ A.classList classListContent
                    , A.attribute "aria-labelledby" id
                    , Attr.role "region"
                    , A.id regionId
                    , Attr.attributeIf (not open) (A.attribute "inert" "true")
                    ]
                    [ viewHorizontalItem
                        [ viewLabelTime "Gyldig fra" fareContract.validFrom timeZone
                        , viewLabelTime "Gyldig til" fareContract.validTo timeZone
                        ]
                    , H.div [ A.class "ui-ticketDetails__item ui-ticketDetails__item--horizontal ui-ticketDetails__item--statusLine" ]
                        [ Ui.LabelItem.viewCompact "Kjøpstidspunkt" [ H.text <| Util.Format.dateTime fareContract.created ]
                        , Ui.LabelItem.viewCompact "Betalt med" [ H.text <| formatPaymentType fareContract.paymentType ]
                        , Ui.LabelItem.viewCompact "Ordre-ID" [ H.text fareContract.orderId ]
                        ]
                    ]
                ]
            ]


viewTicketButtonTextAndIcon : TicketDetails msg -> List (Html msg)
viewTicketButtonTextAndIcon { fareContract, currentTime, timeZone } =
    let
        now =
            Time.posixToMillis currentTime

        isCurrentlyActive =
            fareContract.validFrom <= now

        firstCarnet =
            getFirstCarnetType fareContract

        classListButtonTitle =
            [ ( "ui-ticketDetails__headerButton__title", True )
            , ( "ui-ticketDetails__headerButton__title--active", isCurrentlyActive )
            ]

        icon =
            if isCurrentlyActive then
                Icon.ticketLargeValid

            else
                Icon.ticketLargeWaiting
    in
        case firstCarnet of
            Just carnet ->
                [ H.span [ A.class "ui-ticketDetails__headerButton__icon" ]
                    [ Icon.viewLargeMonochrome Icon.tickets ]
                , H.span [ A.classList classListButtonTitle ]
                    [ viewCarnetHeader carnet classListButtonTitle ]
                ]

            _ ->
                [ H.span [ A.class "ui-ticketDetails__headerButton__icon" ]
                    [ icon ]
                , H.span [ A.classList classListButtonTitle ]
                    [ viewValidity fareContract.validFrom fareContract.validTo currentTime timeZone ]
                ]


viewCarnetHeader : TravelRightCarnet -> List ( String, Bool ) -> Html msg
viewCarnetHeader carnetType classListButtonTitle =
    let
        numberUsed =
            String.fromInt <| carnetType.maximumNumberOfAccesses - carnetType.numberOfUsedAccesses
    in
        H.span [ A.classList (( "ui-ticketDetails__headerButton__title--carnet", True ) :: classListButtonTitle) ]
            [ H.text <| numberUsed ++ " klipp igjen" ]


viewActivation : ( Reservation, ReservationStatus ) -> Html msg
viewActivation ( reservation, status ) =
    let
        classList =
            [ ( "ui-ticketDetails", True )
            ]

        classListMetadata =
            [ ( "ui-ticketDetails__metaDataitem", True )
            , ( "ui-ticketDetails__metaDataitem--padded", True )
            ]

        icon =
            activeReservationLoading

        captureText =
            case status of
                Captured ->
                    "Betaling godkjent. Henter billett..."

                NotCaptured ->
                    "Prosesseres... ikke gyldig enda."
    in
        Ui.TextContainer.primary
            [ H.section
                [ A.classList classList ]
                [ H.h2 [ A.class "ui-ticketDetails__header" ]
                    [ H.div [ A.class "ui-ticketDetails__headerButton" ]
                        [ H.div [ A.class "ui-ticketDetails__headerButton__icon" ]
                            [ icon ]
                        , H.div [ A.class "ui-ticketDetails__headerButton__title" ]
                            [ H.text captureText ]
                        ]
                    ]
                , H.div [ A.classList classListMetadata ]
                    [ Ui.LabelItem.viewCompact
                        "Ordre-ID"
                        [ H.text reservation.orderId ]
                    ]
                ]
            ]


viewTravelRightSummary : Shared -> TravelRightSummary -> Html msg
viewTravelRightSummary shared travelRight =
    let
        product =
            shared.fareProducts
                |> List.Extra.find
                    (\entry ->
                        entry.id == travelRight.fareProductRef
                    )
    in
        case product of
            Nothing ->
                Html.Extra.nothing

            Just p ->
                viewHorizontalItem
                    [ H.div [ A.class "ui-ticketDetails__summaryBox" ]
                        [ H.p [] [ H.text <| langString p.name ]
                        , viewZones shared travelRight.zones
                        ]
                    , H.div []
                        (viewUserProfiles
                            shared
                            travelRight.userProfilesRefs
                        )
                    ]


viewUserProfiles : Shared -> List ( String, Int ) -> List (Html msg)
viewUserProfiles shared userProfileRefs =
    let
        maybeUserProfile =
            userProfileRefs
                |> List.map
                    (Tuple.mapFirst
                        (\ref ->
                            shared.userProfiles
                                |> List.Extra.find
                                    (\entry ->
                                        entry.id == ref
                                    )
                        )
                    )
    in
        maybeUserProfile
            |> List.map
                (\( maybeUser, count ) ->
                    case maybeUser of
                        Nothing ->
                            Html.Extra.nothing

                        Just userProfile ->
                            H.p [] [ H.text <| (String.fromInt count ++ " " ++ langString userProfile.name) ]
                )


viewZones : Shared -> List String -> Html msg
viewZones shared zoneRefs =
    let
        zones =
            zoneRefs
                |> List.map
                    (\ref ->
                        shared.tariffZones
                            |> List.Extra.find (.id >> (==) ref)
                            |> Maybe.map (\zone -> langString zone.name)
                            |> Maybe.withDefault ""
                    )
                |> List.filter ((/=) "")

        numZones =
            List.length zones

        firstZone =
            List.head zones

        lastZone =
            List.Extra.last zones

        zoneString =
            if firstZone == lastZone || lastZone == Nothing then
                "Reise i 1 sone (Sone " ++ Maybe.withDefault "" firstZone ++ ")"

            else
                "Reise i " ++ pluralFormat numZones "sone" "soner" ++ " (Sone " ++ Maybe.withDefault "" firstZone ++ " til " ++ Maybe.withDefault "" lastZone ++ ")"
    in
        H.p [] [ H.text zoneString ]


viewLabelTime : String -> Int -> Time.Zone -> Html msg
viewLabelTime title dateTime timeZone =
    Ui.LabelItem.viewCompact title
        [ dateTime
            |> Time.millisToPosix
            |> TimeUtil.toFullHumanized timeZone
            |> H.text
        ]


viewHorizontalItem : List (Html msg) -> Html msg
viewHorizontalItem =
    H.div [ A.class "ui-ticketDetails__item ui-ticketDetails__item--horizontal" ]


groupTravelRights : List TravelRightFull -> List TravelRightSummary
groupTravelRights travelRights =
    travelRights
        |> Dict.Extra.groupBy (\travelRight -> travelRight.fareProductRef)
        |> Dict.map
            (\key value ->
                let
                    userProfileCount =
                        value
                            |> List.map .userProfileRef
                            |> frequency
                            |> Dict.toList

                    zones =
                        value
                            |> List.head
                            |> Maybe.map .tariffZoneRefs
                            |> Maybe.withDefault []
                in
                    { zones = zones
                    , fareProductRef = key
                    , userProfilesRefs = userProfileCount
                    }
            )
        |> Dict.values


getFirstCarnetType : FareContract -> Maybe TravelRightCarnet
getFirstCarnetType fareContract =
    fareContract.travelRights
        |> List.filterMap
            (\travelRight ->
                case travelRight of
                    CarnetTicket x ->
                        Just x

                    _ ->
                        Nothing
            )
        |> List.head


onlyTravelRightFull : List TravelRight -> List TravelRightFull
onlyTravelRightFull travelRights =
    List.filterMap
        (\travelRight ->
            case travelRight of
                SingleTicket x ->
                    Just x

                PeriodTicket x ->
                    Just x

                CarnetTicket _ ->
                    Nothing

                UnknownTicket _ ->
                    Nothing
        )
        travelRights


formatPaymentType : List String -> String
formatPaymentType types =
    case types of
        [ "VIPPS" ] ->
            "Vipps"

        [ "VISA" ] ->
            "Bankkort"

        _ ->
            "Ukjent"


boolAsString : Bool -> String
boolAsString b =
    if b then
        "true"

    else
        "false"


viewValidity : Int -> Int -> Time.Posix -> Time.Zone -> Html msg
viewValidity from to posixNow timeZone =
    let
        now =
            Time.posixToMillis posixNow
    in
        if from > now + (3 * 60 * 60 * 1000) then
            -- Active in over 3 hours, show absolute time.
            H.text <| "Gyldig fra " ++ TimeUtil.toFullHumanized timeZone (Time.millisToPosix from)

        else if from > now then
            -- If active within 3 hours, show relative time
            H.text <| "Gyldig om " ++ timeFormat ((from - now) // 1000) "" "kort tid"

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


activeReservationLoading : Html msg
activeReservationLoading =
    H.div [ A.class "ui-ticketDetails__activeReservationLoading" ] []
