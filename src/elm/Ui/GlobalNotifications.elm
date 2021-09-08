module Ui.GlobalNotifications exposing (notifications)

import Html as H exposing (Html)
import Html.Attributes as A
import Html.Extra
import Notification exposing (Notification)
import Ui.Section


notifications : List (Notification msg) -> Html msg
notifications messages =
    case messages of
        [] ->
            Html.Extra.nothing

        _ ->
            H.div
                [ A.class "ui-notifications" ]
                (messages
                    |> List.map
                        (\msg ->
                            Ui.Section.init
                                |> Ui.Section.setMarginBottom True
                                |> Ui.Section.viewWithOptions [ msg.content ]
                        )
                )
