module Ui.GlobalNotifications exposing (notifications)

import Html as H exposing (Html)
import Html.Attributes as A
import Notification exposing (Notification)
import Ui.Section


notifications : List (Notification msg) -> Html msg
notifications messages =
    H.div
        [ A.class "ui-notifications" ]
        (messages
            |> List.map .content
            |> List.map
                (\e ->
                    Ui.Section.init
                        |> Ui.Section.setMarginBottom True
                        |> Ui.Section.viewWithOptions [ e ]
                )
        )
