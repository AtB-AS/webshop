module Page.History exposing (Model, Msg, init, subscriptions, update, view)

import Base exposing (AppInfo)
import Data.RefData exposing (LangString(..))
import Environment exposing (Environment)
import Fragment.Icon as Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import PageUpdater exposing (PageUpdater)
import Route exposing (Route)
import Shared exposing (Shared)


type Msg
    = InputFrom String
    | InputTo String
    | ToggleOrder String


type alias Model =
    { from : Maybe String
    , to : Maybe String
    , orders : List FareContract
    , expanded : Maybe String
    }


{-| TODO: Move this.

decode from a string ("type" on the travel right object):

  - PreActivatedPeriodTicket
  - PreActivatedSingleTicket
  - UnknownTicket

UnknownTicket only has id and status

-}
type TravelRight
    = SingleTicket TravelRightFull
    | PeriodTicket TravelRightFull
    | UnknownTicket TravelRightBase


{-| TODO: Move this.

xDateTime are of type time.Time

-}
type alias TravelRightFull =
    { id : String
    , status : Int
    , fareProductRef : String
    , startDateTime : String
    , endDateTime : String
    , usageValidityPeriodRef : String
    , userProfileRef : String
    , authorityRef : String
    , tariffZoneRefs : List String
    }


{-| TODO: Move this.
-}
type alias TravelRightBase =
    { id : String
    , status : Int
    }


{-| TODO: Move this.

created is of type time.Time

-}
type alias FareContract =
    { created : String
    , version : String
    , orderId : String
    , minimumSecurityLevel : Int
    , id : String
    , travelRights : List TravelRight
    , state : Int
    , qrCode : Maybe String
    }


init : Model
init =
    { from = Nothing
    , to = Nothing
    , orders =
        [ { created = "2021-03-12T10:04:06Z"
          , version = "1"
          , orderId = "XWU3JAPE"
          , minimumSecurityLevel = -200
          , id = "ATB:FareContract:XWU3JAPE"
          , travelRights =
                [ SingleTicket
                    { id = "ATB:CustomerPurchasePackage:59838926-d8b2-4020-9aa9-c616f40722b9"
                    , status = 5
                    , fareProductRef = "ATB:PreassignedFareProduct:8808c360"
                    , startDateTime = "2021-03-12T10:04:06Z"
                    , endDateTime = "2021-03-12T11:34:06Z"
                    , usageValidityPeriodRef = "ATB:UsageValidityPeriod:72a05229"
                    , userProfileRef = "ATB:UserProfile:d3e4ec09"
                    , authorityRef = "ATB:Authority:1"
                    , tariffZoneRefs = [ "ATB:TariffZone:1" ]
                    }
                ]
          , state = 2
          , qrCode = Just "CisIAhIkMWNhNTY1ODktMDliNS00OGFhLWI2MGEtNWE0MGRlODBmMWQyKgEEEu0CUj8wPQIdAJGjxdDNc0RhGE220IGEd500iun2wYGWxH/D5HQCHCi4o/yexLPc0snVVotad9Zyx7QQJAHB8Uod0rdapgIwggEiMIHKoAMCAQICBgF4Ja3BmzAKBggqhkjOPQQDAjAOMQwwCgYDVQQDDANhYnQwHhcNMjEwMzEyMDkwNDA3WhcNMjEwMzEzMDkwNDA3WjAvMS0wKwYDVQQDDCQxY2E1NjU4OS0wOWI1LTQ4YWEtYjYwYS01YTQwZGU4MGYxZDIwTjAQBgcqhkjOPQIBBgUrgQQAIQM6AAT6HlFyaECcyvvYrg3fn2mGpeYrCUZhVRgjZDceLDpHoqeYIDvycb0wtLxXhSEgCWru4jjBfecxvjAKBggqhkjOPQQDAgNHADBEAiA2Z9B1zlOh7cEDC4aitecJXS/uB7RzFPM9vGNRulJF6AIgV32lE3BKUyvays7ITNSSZNP7X4+YWjc3HpMCbE5bKspiAU4="
          }
        , { created = "2021-03-10T10:04:06Z"
          , version = "1"
          , orderId = "XWU1JAPE"
          , minimumSecurityLevel = -200
          , id = "ATB:FareContract:XWU1JAPE"
          , travelRights =
                [ SingleTicket
                    { id = "ATB:CustomerPurchasePackage:59838926-d8b2-4020-9aa9-c616f40722b9"
                    , status = 5
                    , fareProductRef = "ATB:PreassignedFareProduct:8808c360"
                    , startDateTime = "2021-03-10T10:04:06Z"
                    , endDateTime = "2021-03-10T11:34:06Z"
                    , usageValidityPeriodRef = "ATB:UsageValidityPeriod:72a05229"
                    , userProfileRef = "ATB:UserProfile:d3e4ec09"
                    , authorityRef = "ATB:Authority:1"
                    , tariffZoneRefs = [ "ATB:TariffZone:1" ]
                    }
                , SingleTicket
                    { id = "ATB:CustomerPurchasePackage:59838926-d8b2-4020-9aa9-c616f40722b9"
                    , status = 5
                    , fareProductRef = "ATB:PreassignedFareProduct:8808c360"
                    , startDateTime = "2021-03-10T10:04:06Z"
                    , endDateTime = "2021-03-10T11:34:06Z"
                    , usageValidityPeriodRef = "ATB:UsageValidityPeriod:72a05229"
                    , userProfileRef = "ATB:UserProfile:d3e4ec09"
                    , authorityRef = "ATB:Authority:1"
                    , tariffZoneRefs = [ "ATB:TariffZone:1" ]
                    }
                ]
          , state = 2
          , qrCode = Just "CisIAhIkMWNhNTY1ODktMDliNS00OGFhLWI2MGEtNWE0MGRlODBmMWQyKgEEEu0CUj8wPQIdAJGjxdDNc0RhGE220IGEd500iun2wYGWxH/D5HQCHCi4o/yexLPc0snVVotad9Zyx7QQJAHB8Uod0rdapgIwggEiMIHKoAMCAQICBgF4Ja3BmzAKBggqhkjOPQQDAjAOMQwwCgYDVQQDDANhYnQwHhcNMjEwMzEyMDkwNDA3WhcNMjEwMzEzMDkwNDA3WjAvMS0wKwYDVQQDDCQxY2E1NjU4OS0wOWI1LTQ4YWEtYjYwYS01YTQwZGU4MGYxZDIwTjAQBgcqhkjOPQIBBgUrgQQAIQM6AAT6HlFyaECcyvvYrg3fn2mGpeYrCUZhVRgjZDceLDpHoqeYIDvycb0wtLxXhSEgCWru4jjBfecxvjAKBggqhkjOPQQDAgNHADBEAiA2Z9B1zlOh7cEDC4aitecJXS/uB7RzFPM9vGNRulJF6AIgV32lE3BKUyvays7ITNSSZNP7X4+YWjc3HpMCbE5bKspiAU4="
          }
        ]
    , expanded = Nothing
    }


update : Msg -> Environment -> Model -> PageUpdater Model Msg
update msg env model =
    case msg of
        InputFrom value ->
            PageUpdater.init { model | from = Just value }

        InputTo value ->
            PageUpdater.init { model | to = Just value }

        ToggleOrder id ->
            PageUpdater.init
                { model
                    | expanded =
                        if model.expanded == Just id then
                            Nothing

                        else
                            Just id
                }


view : Environment -> AppInfo -> Shared -> Model -> Maybe Route -> Html Msg
view env _ shared model _ =
    H.div [ A.class "page-history" ]
        [ H.div [ A.class "sidebar" ] [ viewSidebar model ]
        , H.div [ A.class "main" ] [ viewMain shared model ]
        ]


viewSidebar : Model -> Html Msg
viewSidebar model =
    H.div [ A.class "section-box" ]
        [ H.div [ A.class "section-header" ] [ H.text "Filtrer på dato" ]
        , textInput (Maybe.withDefault "" model.from)
            InputFrom
            "Fra"
            "Velg dato"
        , textInput (Maybe.withDefault "" model.to)
            InputTo
            "Til"
            "Velg dato"
        ]


viewMain : Shared -> Model -> Html Msg
viewMain shared model =
    H.div [ A.class "main" ]
        [ if List.isEmpty model.orders then
            H.div [] [ H.text "No tickets" ]

          else
            H.div [] <| List.map (viewOrder shared model) model.orders
        ]


langString : LangString -> String
langString (LangString _ value) =
    value


fareProductName : Shared -> String -> String
fareProductName shared ref =
    shared.fareProducts
        |> List.filter (.id >> (==) ref)
        |> List.head
        |> Maybe.map (.name >> langString)
        |> Maybe.withDefault "Ukjent product"


viewOrder : Shared -> Model -> FareContract -> Html Msg
viewOrder shared model order =
    let
        fareProduct =
            order.travelRights
                |> List.head
                |> Maybe.map
                    (\travelRight ->
                        case travelRight of
                            SingleTicket data ->
                                fareProductName shared data.fareProductRef

                            PeriodTicket data ->
                                fareProductName shared data.fareProductRef

                            UnknownTicket _ ->
                                "Ukjent"
                    )
                |> Maybe.withDefault "Ukjent"

        travellers =
            case List.length order.travelRights of
                0 ->
                    ""

                1 ->
                    ""

                n ->
                    ", " ++ String.fromInt n ++ " reisende"

        created =
            --order.created
            "12.03.2021"

        expanded =
            model.expanded == Just order.id
    in
        H.div [ A.class "section-box" ]
            (H.div [ A.class "order-header", E.onClick (ToggleOrder order.id) ]
                [ H.div [] [ H.text <| created ++ " - " ++ fareProduct ++ travellers ]
                , H.div [ A.style "display" "flex" ] <|
                    if expanded then
                        [ H.span [ A.style "margin-right" "12px" ] [ H.text "Skjul" ]
                        , Icon.wrapper 20 Icon.upArrow
                        ]

                    else
                        [ H.span [ A.style "margin-right" "12px" ] [ H.text "Vis" ]
                        , Icon.wrapper 20 Icon.downArrow
                        ]
                ]
                :: (if expanded then
                        [ H.div []
                            [ H.label [] [ H.text "Kjøpsinformasjon" ]
                            , H.div [ A.class "metadata-list" ]
                                [ H.div [] [ H.text "Kjøpt 12.03.2021 - 09:41" ]
                                , H.div [] [ H.text "Totalt kr 80,00" ]
                                , H.div [] [ H.text "Betalt med Vipps" ]
                                , H.div [] [ H.text <| "Ordre-ID: " ++ order.orderId ]
                                ]
                            ]
                        , H.div []
                            [ H.label [] [ H.text "Billett 1 / 3" ]
                            , H.div [ A.class "ticket-list" ]
                                [ H.div [] [ H.text fareProduct ]
                                , H.div [] [ H.text "Voksen" ]
                                , H.div [] [ H.text "Sone A" ]
                                ]
                            ]
                        , H.div []
                            [ H.label [] [ H.text "Billett 2 / 3" ]
                            , H.div [ A.class "ticket-list" ]
                                [ H.div [] [ H.text fareProduct ]
                                , H.div [] [ H.text "Voksen" ]
                                , H.div [] [ H.text "Sone A" ]
                                ]
                            ]
                        , H.div []
                            [ H.label [] [ H.text "Billett 3 / 3" ]
                            , H.div [ A.class "ticket-list" ]
                                [ H.div [] [ H.text fareProduct ]
                                , H.div [] [ H.text "Voksen" ]
                                , H.div [] [ H.text "Sone A" ]
                                ]
                            ]
                        , H.div [ A.style "display" "flex" ]
                            [ H.div
                                [ A.style "flex-grow" "1"
                                , A.style "font-weight" "500"
                                ]
                                [ H.text "Be om kvittering på e-post" ]
                            , Icon.rightArrow
                            ]
                        ]

                    else
                        []
                   )
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- INTERNAL


textInput : String -> (String -> msg) -> String -> String -> Html msg
textInput value action title placeholder =
    H.div []
        [ H.label [] [ H.text title ]
        , H.div []
            [ H.input
                [ A.type_ "text"
                , A.placeholder placeholder
                , E.onInput action
                , A.value value
                ]
                []
            ]
        ]
