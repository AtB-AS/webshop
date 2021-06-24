module Page.Shop.Common exposing (CommonModel, TravelDateTime(..), viewSummary, viewZones)

import Data.RefData exposing (LangString(..), TariffZone)
import Data.Ticket exposing (Offer)
import Fragment.Icon as Icon
import Html as H exposing (Html)
import Html.Attributes as A
import Page.Shop.Summary as SummaryPage
import Shared exposing (Shared)
import Ui.Button as B exposing (ThemeColor(..))
import Ui.Input.Select as Select
import Ui.LabelItem
import Ui.LoadingText
import Ui.Message as Message
import Ui.Section as Section
import Util.Format
import Util.Func as Func
import Util.Status exposing (Status(..))


type TravelDateTime
    = TravelNow
    | TravelFuture (Maybe String)


type alias CommonModel a =
    { a
        | offers : Status (List Offer)
        , product : Maybe String
        , fromZone : Maybe String
        , toZone : Maybe String
        , travelDateTime : TravelDateTime
    }


viewSummary : Shared -> CommonModel a -> Bool -> msg -> Html msg
viewSummary shared model disableButtons onToSummaryClick =
    let
        emptyOffers =
            case model.offers of
                Loaded offers ->
                    List.isEmpty offers

                _ ->
                    False

        summary =
            case model.offers of
                Loaded offers ->
                    Just <|
                        SummaryPage.makeSummary
                            { productId = Maybe.withDefault "" model.product
                            , fromZoneId = Maybe.withDefault "" model.fromZone
                            , toZoneId = Maybe.withDefault "" model.toZone
                            , travelDate = stringFromtravelDateTime model.travelDateTime
                            }
                            offers
                            shared

                _ ->
                    Nothing
    in
        Section.init
            |> Section.viewWithOptions
                [ if emptyOffers then
                    Message.warning "Finner ingen tilgjengelige billetter."

                  else
                    Section.viewPaddedItem
                        [ Ui.LabelItem.viewHorizontal
                            "Total:"
                            [ H.p [ A.class "shop__summaryPrice" ]
                                [ summary
                                    |> Maybe.map .totalPrice
                                    |> Maybe.map (Func.flip Util.Format.float 2)
                                    |> Maybe.map H.text
                                    |> Maybe.withDefault (Ui.LoadingText.view "1.6875rem" "5rem")
                                , H.small [] [ H.text "kr" ]
                                ]
                            ]
                        , Ui.LabelItem.viewHorizontal "Hvorav mva:"
                            [ summary
                                |> Maybe.map .totalVat
                                |> Maybe.map (Func.flip Util.Format.float 2)
                                |> Maybe.map (Func.flip (++) " kr")
                                |> Maybe.map H.text
                                |> Maybe.withDefault (Ui.LoadingText.view "1rem" "3rem")
                            ]
                        ]
                , B.init "Gå til oppsummering"
                    |> B.setDisabled disableButtons
                    |> B.setIcon (Just <| Icon.viewMonochrome Icon.rightArrow)
                    |> B.setOnClick (Just onToSummaryClick)
                    |> B.primary Primary_2
                ]


viewZones : CommonModel a -> String -> List TariffZone -> (String -> msg) -> (String -> msg) -> Html msg
viewZones model defaultZone zones onFromZoneSelected onToZoneSelected =
    let
        sortedZones =
            List.sortWith
                (\a b ->
                    case ( a.name, b.name ) of
                        ( LangString _ nameA, LangString _ nameB ) ->
                            compare nameA nameB
                )
                zones

        selectedFromZone =
            Maybe.withDefault defaultZone model.fromZone

        selectedToZone =
            Maybe.withDefault defaultZone model.toZone
    in
        Section.viewItem
            [ Section.viewHorizontalGroup
                [ Select.init "travelFromZone"
                    |> Select.setTitle (Just "Avreisesone")
                    |> Select.setOnInput (Just onFromZoneSelected)
                    |> Select.view (List.map (viewZone selectedFromZone) sortedZones)
                , Select.init "travelToZone"
                    |> Select.setTitle (Just "Ankomstsone")
                    |> Select.setOnInput (Just onToZoneSelected)
                    |> Select.view (List.map (viewZone selectedToZone) sortedZones)
                ]
            , Section.viewPaddedItem [ H.p [] [ H.a [ A.href "https://atb.no/soner", A.target "_blank" ] [ H.text "Se sonekart og beskrivelser (åpner ny side)" ] ] ]
            ]


viewZone : String -> TariffZone -> Html msg
viewZone current zone =
    H.option
        [ A.value zone.id
        , A.selected (current == zone.id)
        ]
        [ H.text <| langString zone.name ]


langString : LangString -> String
langString (LangString _ value) =
    value


stringFromtravelDateTime : TravelDateTime -> Maybe String
stringFromtravelDateTime travelDateTime =
    case travelDateTime of
        TravelFuture (Just time) ->
            Just time

        _ ->
            Nothing
