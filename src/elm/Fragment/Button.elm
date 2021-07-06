module Fragment.Button exposing (loading)

import Html exposing (Html)
import Svg as S
import Svg.Attributes as SA


loading : Html msg
loading =
    S.svg [ SA.width "40", SA.height "10", SA.viewBox "0 0 120 30", SA.fill "#fff" ]
        [ S.title [] [ S.text "Laster" ]
        , S.circle [ SA.cx "15", SA.cy "15", SA.r "15" ]
            [ S.animate
                [ SA.attributeName "r"
                , SA.from "15"
                , SA.to "15"
                , SA.begin "0s"
                , SA.dur "0.8s"
                , SA.values "15;9;15"
                , SA.calcMode "linear"
                , SA.repeatCount "indefinite"
                ]
                []
            , S.animate
                [ SA.attributeName "fill-opacity"
                , SA.from "1"
                , SA.to "1"
                , SA.begin "0s"
                , SA.dur "0.8s"
                , SA.values "1;.5;1"
                , SA.calcMode "linear"
                , SA.repeatCount "indefinite"
                ]
                []
            ]
        , S.circle [ SA.cx "60", SA.cy "15", SA.r "9", SA.fillOpacity "0.3" ]
            [ S.animate
                [ SA.attributeName "r"
                , SA.from "9"
                , SA.to "9"
                , SA.begin "0s"
                , SA.dur "0.8s"
                , SA.values "9;15;9"
                , SA.calcMode "linear"
                , SA.repeatCount "indefinite"
                ]
                []
            , S.animate
                [ SA.attributeName "fill-opacity"
                , SA.from "0.5"
                , SA.to "0.5"
                , SA.begin "0s"
                , SA.dur "0.8s"
                , SA.values ".5;1;.5"
                , SA.calcMode "linear"
                , SA.repeatCount "indefinite"
                ]
                []
            ]
        , S.circle [ SA.cx "105", SA.cy "15", SA.r "15" ]
            [ S.animate
                [ SA.attributeName "r"
                , SA.from "15"
                , SA.to "15"
                , SA.begin "0s"
                , SA.dur "0.8s"
                , SA.values "15;9;15"
                , SA.calcMode "linear"
                , SA.repeatCount "indefinite"
                ]
                []
            , S.animate
                [ SA.attributeName "fill-opacity"
                , SA.from "1"
                , SA.to "1"
                , SA.begin "0s"
                , SA.dur "0.8s"
                , SA.values "1;.5;1"
                , SA.calcMode "linear"
                , SA.repeatCount "indefinite"
                ]
                []
            ]
        ]
