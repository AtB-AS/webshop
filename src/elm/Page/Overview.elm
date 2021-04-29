module Page.Overview exposing (Model, Msg(..), init, subscriptions, update, view)

import Base exposing (AppInfo)
import Data.FareContract exposing (FareContract, TravelRight(..))
import Data.RefData exposing (LangString(..))
import Data.Webshop exposing (Inspection, Token)
import Dict exposing (Dict)
import Environment exposing (Environment)
import Fragment.Button as Button
import Fragment.Icon as Icon
import GlobalActions as GA
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Html.Extra
import Http
import Json.Decode as Decode exposing (Decoder)
import PageUpdater exposing (PageUpdater)
import Route exposing (Route)
import Service.Misc as MiscService
import Service.Ticket as TicketService
import Shared exposing (Shared)
import Task
import Time
import Ui.Button as B
import Ui.Heading
import Ui.Section
import Util.Format as Format
import Util.Maybe
import Util.Status exposing (Status(..))


type Msg
    = ReceiveFareContracts (Result Decode.Error (List FareContract))
    | Receipt String
    | ReceiveReceipt (Result Http.Error ())
    | ReceiveTokenPayloads (Result Decode.Error (List ( String, String )))
    | OpenShop
    | OpenHistory
    | OpenSettings
    | OpenEditTravelCard
    | UpdateTime Time.Posix
    | ToggleTicket String
    | SetPendingOrder String
    | Logout


type alias Model =
    { tickets : List FareContract
    , tokens : List Token
    , tokenPayloads : List ( String, String )
    , travelCardId : String
    , inspection : Status Inspection
    , currentTime : Time.Posix
    , expanded : Maybe String
    , pending : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { tickets = []
      , tokens = []
      , tokenPayloads = []
      , travelCardId = ""
      , inspection = NotLoaded
      , currentTime = Time.millisToPosix 0
      , expanded = Nothing
      , pending = Nothing
      }
    , Cmd.none
    )


update : Msg -> Environment -> Model -> PageUpdater Model Msg
update msg env model =
    case msg of
        ReceiveFareContracts result ->
            case result of
                Ok fareContracts ->
                    let
                        -- Only store tickets that are valid into the future.
                        tickets =
                            fareContracts
                                |> List.filter (\{ validTo } -> isValid validTo model.currentTime)
                                |> List.sortBy (.created >> .timestamp)
                                |> List.reverse

                        pendingDone =
                            fareContracts
                                |> List.filter (\{ orderId } -> model.pending == Just orderId)
                                |> List.isEmpty
                                |> not

                        newPending =
                            if pendingDone then
                                Nothing

                            else
                                model.pending
                    in
                        PageUpdater.init { model | tickets = tickets, pending = newPending }

                Err _ ->
                    PageUpdater.init model

        Receipt orderId ->
            PageUpdater.fromPair ( model, sendReceipt env orderId )

        ReceiveReceipt result ->
            case result of
                Ok () ->
                    PageUpdater.init model

                Err _ ->
                    PageUpdater.init model

        ReceiveTokenPayloads result ->
            case result of
                Ok value ->
                    PageUpdater.init { model | tokenPayloads = value }

                Err _ ->
                    PageUpdater.init { model | tokenPayloads = [] }

        OpenShop ->
            PageUpdater.init model
                |> PageUpdater.addGlobalAction GA.OpenShop

        OpenHistory ->
            PageUpdater.init model
                |> PageUpdater.addGlobalAction (GA.RouteTo Route.History)

        OpenSettings ->
            PageUpdater.init model
                |> PageUpdater.addGlobalAction (GA.RouteTo (Route.Settings Route.Overview))

        OpenEditTravelCard ->
            PageUpdater.init model
                |> PageUpdater.addGlobalAction (GA.RouteTo (Route.Settings Route.EditTravelCard))
                |> PageUpdater.addGlobalAction (GA.FocusItem (Just "tkort"))

        UpdateTime posixTime ->
            PageUpdater.init { model | currentTime = posixTime }

        ToggleTicket id ->
            PageUpdater.init
                { model
                    | expanded =
                        if model.expanded == Just id then
                            Nothing

                        else
                            Just id
                }

        SetPendingOrder orderId ->
            PageUpdater.init { model | pending = Just orderId }

        Logout ->
            PageUpdater.init model
                |> PageUpdater.addGlobalAction GA.Logout


view : Environment -> AppInfo -> Shared -> Model -> Maybe Route -> Html Msg
view env _ shared model _ =
    case env.customerId of
        Just _ ->
            H.div [ A.class "page-overview" ]
                [ viewSidebar shared model
                , viewMain shared model
                ]

        Nothing ->
            H.text ""


viewSidebar : Shared -> Model -> Html Msg
viewSidebar shared model =
    H.div [ A.class "sidebar" ]
        [ viewAccountInfo shared model
        , viewActions model
        ]


viewAccountInfo : Shared -> Model -> Html Msg
viewAccountInfo shared _ =
    Ui.Section.init
        |> Ui.Section.setMarginBottom True
        |> Ui.Section.viewWithOptions
            [ Ui.Section.viewPaddedItem
                [ Ui.Heading.component "Min profil"
                , Html.Extra.viewMaybe (\d -> H.p [] [ H.text d.phone ]) shared.profile
                , shared.profile
                    |> Util.Maybe.flatMap .travelCard
                    |> Maybe.map .id
                    |> Html.Extra.viewMaybe
                        (\id ->
                            H.p []
                                [ H.text "t:kort – "
                                , H.text <| String.fromInt id
                                ]
                        )
                ]
            , B.init "Rediger profil"
                |> B.setDisabled False
                |> B.setIcon (Just Icon.edit)
                |> B.setOnClick (Just OpenSettings)
                |> B.tertiary
            , case Util.Maybe.flatMap .travelCard shared.profile of
                Just _ ->
                    Html.Extra.nothing

                _ ->
                    B.init "Legg til t:kort "
                        |> B.setDisabled False
                        |> B.setIcon (Just Icon.travelCard)
                        |> B.setOnClick (Just OpenEditTravelCard)
                        |> B.tertiary
            ]


viewActions : Model -> Html Msg
viewActions _ =
    Ui.Section.view
        [ B.init "Kjøp ny billett"
            |> B.setDisabled False
            |> B.setIcon (Just Icon.ticketAdd)
            |> B.setOnClick (Just OpenShop)
            |> B.tertiary
        , B.init "Kjøpshistorikk"
            |> B.setDisabled False
            |> B.setIcon (Just Icon.tickets)
            |> B.setOnClick (Just OpenHistory)
            |> B.tertiary
        ]


viewMain : Shared -> Model -> Html Msg
viewMain shared model =
    let
        tickets =
            List.filter
                (\{ validTo } -> isValid validTo model.currentTime)
                model.tickets
    in
        H.div [ A.class "main" ]
            [ if List.isEmpty tickets && model.pending == Nothing then
                H.div [] [ H.text "Ingen billetter er tilknyttet din konto." ]

              else
                H.div [] (viewPending model :: viewTicketCards shared model)
            ]


viewPending : Model -> Html msg
viewPending model =
    case model.pending of
        Just _ ->
            H.div [ A.class "section-box" ]
                [ H.div [ A.class "ticket-header" ]
                    [ Icon.wrapper 20 Icon.bus
                    , H.div [ A.class "product-name" ] [ H.text "Utsteder billett..." ]
                    ]
                , H.div [ A.class "ticket-progress" ] []
                , H.div [ A.class "ticket-waiting" ] [ Button.loading ]
                ]

        Nothing ->
            H.text ""


viewTicketCards : Shared -> Model -> List (Html Msg)
viewTicketCards shared model =
    model.tickets
        |> List.filter (\{ validTo } -> isValid validTo model.currentTime)
        |> List.map (viewTicketCard shared model)


timeLeft : Int -> String
timeLeft time =
    let
        days =
            time // 86400

        hr =
            (time - days * 86400) // 3600

        min =
            (time - days * 86400 - hr * 3600) // 60

        sec =
            time - days * 86400 - hr * 3600 - min * 60

        toStr v suffix =
            if v > 0 then
                String.fromInt v ++ suffix

            else
                ""
    in
        String.join " " [ toStr days "d", toStr hr "h", toStr min "m", toStr sec "s" ]


timeAgo : Int -> String
timeAgo time =
    if time >= 86400 then
        String.fromInt (time // 86400) ++ "d"

    else if time >= 3600 then
        String.fromInt (time // 3600) ++ "h"

    else if time >= 60 then
        String.fromInt (time // 60) ++ "m"

    else if time >= 1 then
        String.fromInt time ++ "s"

    else
        "just now"


pluralFormat : Int -> String -> String -> String
pluralFormat value singular plural =
    String.fromInt value
        ++ " "
        ++ (if value == 1 then
                singular

            else
                plural
           )


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


timeLeftFormat : Int -> String
timeLeftFormat time =
    timeFormat time "igjen" "utløper straks"


timeAgoFormat : Int -> String
timeAgoFormat time =
    timeFormat time "siden" "nå nettopp"


frequency : List comparable -> Dict comparable Int
frequency =
    List.foldl
        (\item ->
            Dict.update item (Maybe.map ((+) 1) >> Maybe.withDefault 1 >> Just)
        )
        Dict.empty


viewValidity : Int -> Time.Posix -> Html msg
viewValidity to posixNow =
    let
        now =
            Time.posixToMillis posixNow
    in
        if now > to then
            H.text <| timeAgoFormat <| (now - to) // 1000

        else
            H.text <| timeLeftFormat <| (to - now) // 1000


isValid : Int -> Time.Posix -> Bool
isValid to posixNow =
    to >= Time.posixToMillis posixNow


viewTicketCard : Shared -> Model -> FareContract -> Html Msg
viewTicketCard shared model fareContract =
    let
        expanded =
            model.expanded == Just fareContract.orderId

        userProfilesList =
            fareContract.travelRights
                |> List.filterMap
                    (\type_ ->
                        case type_ of
                            SingleTicket ticket ->
                                Just ticket.userProfileRef

                            PeriodTicket ticket ->
                                Just ticket.userProfileRef

                            UnknownTicket _ ->
                                Nothing
                    )

        userProfiles =
            userProfilesList
                |> frequency
                |> Dict.map (viewUserProfile shared)
                |> Dict.values

        userInfo =
            case userProfiles of
                [ userProfile ] ->
                    userProfile

                [ _, _ ] ->
                    String.join ", " userProfiles

                _ ->
                    String.fromInt (List.length userProfilesList) ++ " tickets"

        fareProduct =
            fareContract.travelRights
                |> List.filterMap
                    (\type_ ->
                        case type_ of
                            SingleTicket ticket ->
                                Just ticket.fareProductRef

                            PeriodTicket ticket ->
                                Just ticket.fareProductRef

                            UnknownTicket _ ->
                                Nothing
                    )
                |> frequency
                |> Dict.map (viewFareProduct shared)
                |> Dict.values
                |> List.filter ((/=) "")
                |> String.join ", "

        zones =
            fareContract.travelRights
                |> List.filterMap
                    (\type_ ->
                        case type_ of
                            SingleTicket ticket ->
                                Just ticket.tariffZoneRefs

                            PeriodTicket ticket ->
                                Just ticket.tariffZoneRefs

                            UnknownTicket _ ->
                                Nothing
                    )
                |> List.concat
                |> frequency
                |> Dict.map
                    (\ref _ ->
                        shared.tariffZones
                            |> List.filter (.id >> (==) ref)
                            |> List.head
                            |> Maybe.map (\zone -> "Sone " ++ langString zone.name)
                            |> Maybe.withDefault ""
                    )
                |> Dict.values
                |> List.filter ((/=) "")
    in
        H.div [ A.class "section-box" ]
            [ H.div [ A.class "ticket-header" ]
                [ Icon.wrapper 20 Icon.bus
                , H.div [ A.class "product-name" ]
                    [ if fareProduct == "" then
                        H.text "Ukjent produkt"

                      else
                        H.text fareProduct
                    ]
                , H.div [ A.class "zone-name" ]
                    [ case zones of
                        [] ->
                            H.text "Ingen soner"

                        [ zone ] ->
                            H.text zone

                        _ ->
                            H.text <| String.fromInt (List.length zones) ++ " soner"
                    ]
                ]
            , H.div [ A.class "card-content" ]
                [ H.div
                    [ A.class "ticket-info" ]
                    [ viewValidity fareContract.validTo model.currentTime ]
                ]
            , if expanded then
                H.div []
                    [ H.label [] [ H.text "Kjøpsinformasjon" ]
                    , H.div [ A.class "metadata-list" ]
                        [ H.div [] [ H.text <| "Kjøpt " ++ Format.dateTime fareContract.created ]
                        , H.div [] [ H.text <| "Totalt kr " ++ formatTotal fareContract.totalAmount ]
                        , H.div [] [ H.text <| "Betalt med " ++ formatPaymentType fareContract.paymentType ]
                        , H.div [] [ H.text <| "Ordre-ID: " ++ fareContract.orderId ]
                        ]
                    ]

              else
                H.text ""
            , H.div
                [ A.style "display" "flex"
                , A.style "cursor" "pointer"
                , E.onClick (ToggleTicket fareContract.orderId)
                ]
                (H.div
                    [ A.style "flex-grow" "1"
                    , A.style "font-weight" "500"
                    ]
                    [ H.text "Detaljer" ]
                    :: (if expanded then
                            [ H.span [ A.style "margin-right" "12px" ] [ H.text "Skjul" ]
                            , Icon.wrapper 20 Icon.upArrow
                            ]

                        else
                            [ H.span [ A.style "margin-right" "12px" ] [ H.text "Vis" ]
                            , Icon.wrapper 20 Icon.downArrow
                            ]
                       )
                )
            ]


formatTotal : Maybe String -> String
formatTotal value =
    case Maybe.andThen String.toFloat value of
        Just floatValue ->
            Format.float floatValue 2

        Nothing ->
            "??"


formatPaymentType : List String -> String
formatPaymentType types =
    case types of
        [] ->
            "??"

        [ "VIPPS" ] ->
            "Vipps"

        [ "VISA" ] ->
            "bankkort"

        _ ->
            "??"


langString : LangString -> String
langString (LangString _ value) =
    value


multiString : Int -> String -> String
multiString count str =
    if String.isEmpty str || count < 1 then
        ""

    else if count == 1 then
        str

    else
        String.fromInt count ++ "× " ++ str


viewUserProfile : Shared -> String -> Int -> String
viewUserProfile shared userProfile count =
    shared.userProfiles
        |> List.filter
            (\entry ->
                entry.id == userProfile
            )
        |> List.map (.name >> langString)
        |> List.head
        |> Maybe.withDefault ""
        |> multiString count


viewFareProduct : Shared -> String -> Int -> String
viewFareProduct shared fareProduct _ =
    shared.fareProducts
        |> List.filter
            (\entry ->
                entry.id == fareProduct
            )
        |> List.map (.name >> langString)
        |> List.head
        |> Maybe.withDefault ""
        |> multiString 1


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ MiscService.receiveTokens (Decode.decodeValue tokenPayloadsDecoder >> ReceiveTokenPayloads)
        , MiscService.receiveFareContracts
            (Decode.decodeValue (Decode.list MiscService.fareContractDecoder)
                >> ReceiveFareContracts
            )
        , Time.every 1000 UpdateTime
        ]



-- INTERNAL


tokenPayloadDecoder : Decoder ( String, String )
tokenPayloadDecoder =
    Decode.map2 Tuple.pair (Decode.index 0 Decode.string) (Decode.index 1 Decode.string)


tokenPayloadsDecoder : Decoder (List ( String, String ))
tokenPayloadsDecoder =
    Decode.list tokenPayloadDecoder


sendReceipt : Environment -> String -> Cmd Msg
sendReceipt env orderId =
    if env.customerEmail == "" then
        Cmd.none

    else
        TicketService.receipt env env.customerEmail orderId
            |> Http.toTask
            |> Task.attempt ReceiveReceipt
