module Page.Contact exposing (Model, Msg(..), init, subscriptions, update, view)

import Base exposing (AppInfo)
import Data.RefData exposing (LangString(..))
import Environment exposing (Environment)
import Fragment.Icon as Icon
import GlobalActions as GA exposing (GlobalAction(..))
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Extra
import PageUpdater exposing (PageUpdater)
import Route exposing (Route)
import Service.Misc as MiscService exposing (Profile)
import Shared exposing (Shared)
import Ui.Button as B
import Ui.Heading
import Ui.Section as S
import Ui.TextContainer


type Msg
    = OnEnterPage
    | ProfileChange (Maybe Profile)


type alias Model =
    { profile : Maybe Profile
    }


init : ( Model, Cmd Msg )
init =
    ( { profile = Nothing
      }
    , Cmd.none
    )


update : Msg -> Environment -> Model -> PageUpdater Model Msg
update msg _ model =
    case msg of
        OnEnterPage ->
            PageUpdater.fromPair ( model, Tuple.second init )
                |> (PageUpdater.addGlobalAction <| GA.SetTitle <| Just "Kontakt AtB")
                |> (PageUpdater.addGlobalAction <| GA.FocusItem <| Just "page-header")

        ProfileChange profile ->
            PageUpdater.init
                { model | profile = profile }


view : Environment -> AppInfo -> Shared -> Model -> Maybe Route -> Html Msg
view _ _ _ model _ =
    H.div [ A.class "page page--twoColumns" ]
        [ viewCustomerSupport model
        , viewIntercom model
        ]


viewCustomerSupport : Model -> Html Msg
viewCustomerSupport _ =
    H.div []
        [ S.view
            [ S.viewPaddedItem
                [ Ui.Heading.component "Kontakt AtB" Nothing
                , viewIconSplit (Icon.viewLarge Icon.contact) "For spørsmål om billetter, rutetider eller du trenger hjelp til å komme i dialog med oss."
                ]
            , B.init "Kontakt oss (åpner atb.no)"
                |> B.setElement H.a
                |> B.setIcon (Just Icon.rightArrow)
                |> B.setAttributes [ A.href "" ]
                |> B.primary B.Primary_2
            ]
        ]


viewIntercom : Model -> Html Msg
viewIntercom _ =
    H.div []
        [ S.view
            [ S.viewPaddedItem
                [ Ui.Heading.component "Foreslå forbedringer av nettbutikken" Nothing
                , viewIconSplit (Icon.viewLarge Icon.feedback) "Her kan du sende tilbakemeldinger på nettbutikken. Du kan også rapportere feil."
                ]
            , B.init "Send tilbakemelding til utviklingsteamet"
                |> B.setElement H.a
                |> B.setIcon (Just Icon.rightArrow)
                |> B.setAttributes [ A.href "" ]
                |> B.tertiary
            ]
        ]


viewIconSplit : Html msg -> String -> Html msg
viewIconSplit icon text =
    H.div [ A.class "pageContact__iconSplit" ]
        [ H.div [ A.class "pageContact__iconSplit__image" ] [ icon ]
        , Ui.TextContainer.primary [ H.text text ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ MiscService.onProfileChange ProfileChange
        ]
