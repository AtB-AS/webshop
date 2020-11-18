module Page.Home exposing (Model, Msg, init, subscriptions, update, view)

import Base exposing (AppInfo)
import Data.Ticket exposing (Offer, Ticket)
import Environment exposing (Environment)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Http
import Route exposing (Route)
import Service.Ticket as TicketService
import Service.Webshop as WebshopService
import Task


type Msg
    = FetchTickets
    | ReceiveTickets (Result Http.Error (List Ticket))
    | FetchOffers
    | ReceiveOffers (Result Http.Error (List Offer))
    | Hello
    | ReceiveHello (Result Http.Error ())


type alias Model =
    { tickets : List Ticket
    , offers : List Offer
    , hello : String
    }


init : ( Model, Cmd Msg )
init =
    ( { tickets = []
      , offers = []
      , hello = ""
      }
    , Cmd.none
    )


update : Msg -> Environment -> Model -> ( Model, Cmd Msg )
update msg env model =
    case msg of
        FetchOffers ->
            ( model, fetchOffers env )

        ReceiveOffers result ->
            case result of
                Ok offers ->
                    ( { model | offers = offers }, Cmd.none )

                Err err ->
                    ( model, Cmd.none )

        FetchTickets ->
            ( model
            , env.customerId
                |> Maybe.map (fetchTickets env)
                |> Maybe.withDefault Cmd.none
            )

        ReceiveTickets result ->
            case result of
                Ok tickets ->
                    ( { model | tickets = tickets }, Cmd.none )

                Err err ->
                    ( model, Cmd.none )

        Hello ->
            ( { model | hello = "Trying to say hello..." }, fetchHello env )

        ReceiveHello result ->
            case result of
                Ok () ->
                    ( { model | hello = "Hello was OK" }, Cmd.none )

                Err _ ->
                    ( { model | hello = "Server refused to say hello :(" }, Cmd.none )


view : Environment -> AppInfo -> Model -> Maybe Route -> Html Msg
view env _ model _ =
    case env.customerId of
        Just _ ->
            H.div [ A.class "box" ]
                [ H.h2 [] [ H.text "Hello" ]
                , H.button [ E.onClick Hello ] [ H.text "Hello" ]
                , H.p [] [ H.text model.hello ]
                , H.h2 [] [ H.text "Offers" ]
                , H.button [ E.onClick FetchOffers ] [ H.text "Refresh" ]
                , H.ol [] <| List.map viewOffer model.offers
                , H.h2 [] [ H.text "Tickets" ]
                , H.button [ E.onClick FetchTickets ] [ H.text "Refresh" ]
                , H.ol [] <| List.map viewTicket model.tickets
                ]

        Nothing ->
            H.div [ A.class "box" ]
                [ H.h2 [] [ H.text "Not logged in" ]
                , H.p [] [ H.text "You need to log in." ]
                ]


viewOffer : Offer -> Html msg
viewOffer offer =
    H.div []
        [ H.div [] [ H.text offer.offerId ]
        , H.div [] [ H.text offer.travellerId ]
        , H.div []
            [ offer.prices
                |> List.head
                |> Maybe.map (\price -> price.currency ++ " " ++ price.amount)
                |> Maybe.withDefault "No prices"
                |> H.text
            ]
        ]


viewTicket : Ticket -> Html msg
viewTicket ticket =
    H.text "ticket"


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- INTERNAL


fetchTickets : Environment -> String -> Cmd Msg
fetchTickets env customerId =
    TicketService.getTicketList env customerId
        |> Http.toTask
        |> Task.attempt ReceiveTickets


fetchOffers : Environment -> Cmd Msg
fetchOffers env =
    TicketService.search env
        |> Http.toTask
        |> Task.attempt ReceiveOffers


fetchHello : Environment -> Cmd Msg
fetchHello env =
    WebshopService.hello env
        |> Http.toTask
        |> Task.attempt ReceiveHello
