module Fragment.Icon exposing
    ( atb
    , bus
    , chat
    , checkmark
    , checkmarkCircle
    , creditcard
    , cross
    , delete
    , downArrow
    , duration
    , edit
    , error
    , info
    , leftArrow
    , leftCaret
    , logout
    , rightArrow
    , rightCaret
    , ticket
    , ticketAdd
    , ticketRemove
    , tickets
    , toggleOff
    , toggleOn
    , traveler
    , upArrow
    , vipps
    , warning
    , wrapper
    )

import Html as H exposing (Html)
import Html.Attributes as A
import Svg as S
import Svg.Attributes as SA


{-| Wrap the SVG icon in the given square size.
-}
wrapper : Int -> Html msg -> Html msg
wrapper size iconHtml =
    H.div
        [ A.style "width" (String.fromInt size ++ "px")
        , A.style "height" (String.fromInt size ++ "px")
        , A.style "display" "grid"
        , A.style "place-items" "center"
        ]
        [ iconHtml ]


checkmark : Html msg
checkmark =
    S.svg [ SA.width "21", SA.height "20", SA.viewBox "0 0 21 20", SA.fill "none" ]
        [ S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M8.66707 13.5859L17.96 4.29297L19.3742 5.70718L9.37418 15.7072C8.98365 16.0977 8.35049 16.0977 7.95996 15.7072L2.95996 10.7072L4.37417 9.29297L8.66707 13.5859Z"
            , SA.fill "black"
            ]
            []
        ]


checkmarkCircle : Html msg
checkmarkCircle =
    S.svg [ SA.width "88", SA.height "88", SA.viewBox "0 0 88 88", SA.fill "none" ]
        [ S.path
            [ SA.d "M57.4057 28.6057L39.1998 46.8116L30.5939 38.2057L23.8057 44.9939L35.8057 56.9939C37.6802 58.8684 40.7194 58.8684 42.5939 56.9939L64.1939 35.3939L57.4057 28.6057Z"
            , SA.fill "black"
            ]
            []
        , S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M43.9998 0.799805C20.1411 0.799805 0.799805 20.1411 0.799805 43.9998C0.799805 67.8585 20.1411 87.1998 43.9998 87.1998C67.8585 87.1998 87.1998 67.8585 87.1998 43.9998C87.1998 20.1411 67.8585 0.799805 43.9998 0.799805ZM10.3998 43.9998C10.3998 25.443 25.443 10.3998 43.9998 10.3998C62.5566 10.3998 77.5998 25.443 77.5998 43.9998C77.5998 62.5566 62.5566 77.5998 43.9998 77.5998C25.443 77.5998 10.3998 62.5566 10.3998 43.9998Z"
            , SA.fill "black"
            ]
            []
        ]


atb : Html msg
atb =
    S.svg [ SA.id "logo_atb_graa", SA.viewBox "0 0 383.764 383.657" ]
        [ S.defs [] [ S.style [] [ S.text ".atb-logo-cls-1{fill:#fff;}" ] ]
        , S.path [ SA.id "logo_atb_graa_ramme", SA.class "atb-logo-cls-1", SA.d "M357.187,0h-.009L105.66.11A105.787,105.787,0,0,0,0,105.779l.105,251.31A26.6,26.6,0,0,0,26.68,383.657H357.187a26.607,26.607,0,0,0,26.577-26.577V26.577A26.607,26.607,0,0,0,357.187,0ZM105.669,21.037l251.518-.11a5.619,5.619,0,0,1,3.334,1.095c-7.73,20.742-24.879,35.236-49.262,46.711l-1.871.97C267.95,86.443,218.4,94.728,184.125,98.732c-7.754.906-15.41,1.72-22.821,2.508-35.986,3.828-69.979,7.445-99.445,19.355a140.171,140.171,0,0,0-26.77,14.175l-.809.533q-6.677,4.248-13.337,9.336l-.016-38.86A84.742,84.742,0,0,1,105.669,21.037ZM357.187,362.731H26.68a5.649,5.649,0,0,1-5.648-5.651l-.069-166.018c8.4-8.381,19.625-18.368,32.184-26.342l1.294-.85.228-.154A105.821,105.821,0,0,1,74.951,153c24.933-10.078,56.564-13.443,90.056-17.007,7.493-.8,15.241-1.62,23.174-2.547,36.407-4.252,89.255-13.14,134.3-31.338a178.442,178.442,0,0,0,40.357-22.514V357.08A5.651,5.651,0,0,1,357.187,362.731Z" ] []
        , S.path [ SA.id "logo_atb_graa_tekst", SA.class "atb-logo-cls-1", SA.d "M196.082,331.462c-8.551,0-13.115-1.731-15.562-3.586a15.775,15.775,0,0,1-5.362-8.24,50.139,50.139,0,0,1-1.686-14.394V248.061h-30.1l20.366,83.389H124.431l-3.773-24.819H91.272L87.5,331.45H48.786L82.735,189.687H129.2l8.416,34.349h35.861v-31.57h35.739v31.57h14.1v24.025h-14.1v51.225q0,4.571,2.085,6.452t6.452,1.886h16.086l-.057-117.937h55.645q19.849,0,29.285,9.629t9.432,25.711a45.4,45.4,0,0,1-1.29,10.821,32.191,32.191,0,0,1-3.974,9.531,25.4,25.4,0,0,1-6.848,7.247A25.975,25.975,0,0,1,305.9,256.8v.395a26.633,26.633,0,0,1,11.913,3.673,27.425,27.425,0,0,1,8.14,7.644,32.426,32.426,0,0,1,4.666,10.027,41.727,41.727,0,0,1,1.489,11.02,57.2,57.2,0,0,1-2.382,16.878,34.528,34.528,0,0,1-7.446,13.3,33.532,33.532,0,0,1-13.2,8.638q-8.143,3.078-19.655,3.076H232.929ZM105.768,218.675l-10.126,60.16h20.649l-10.125-60.16Zm173.725,26.6a9.968,9.968,0,0,0,8.737-4.269,17.061,17.061,0,0,0,2.978-10.024,17.881,17.881,0,0,0-2.978-10.326,9.907,9.907,0,0,0-8.737-4.367h-8.934v28.986Zm1.391,59.566a12.153,12.153,0,0,0,9.132-4.269q3.972-4.266,3.971-13.2,0-8.34-3.971-12.709a12.056,12.056,0,0,0-9.132-4.367H270.559v34.548Z" ] []
        ]


bus : Html msg
bus =
    S.svg [ SA.width "18", SA.height "10", SA.viewBox "0 0 18 10", SA.fill "none" ]
        [ S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M17.745 5.99375L16.87 0.62375C16.854 0.520262 16.8017 0.425845 16.7225 0.357374C16.6432 0.288904 16.5422 0.250845 16.4375 0.25H1.125C0.6875 0.25 0.25 0.6975 0.25 1.14625V7.3025C0.25 7.525 0.41 7.75 0.625 7.75H2.55875C2.85375 6.85125 3.69125 6.25 4.6875 6.25C5.68375 6.25 6.5225 6.85125 6.81625 7.75H11.2463C11.5413 6.85125 12.3787 6.25 13.375 6.25C14.3713 6.25 15.2087 6.85125 15.5037 7.75H17.3125C17.5537 7.75 17.75 7.54875 17.75 7.3025V6.0675C17.75 6.0425 17.7475 6.0175 17.745 5.9925V5.99375ZM14.195 2.4875C14.195 2.24 14.3925 2.04 14.6325 2.04H15.9913C16.0987 2.04025 16.2022 2.08001 16.2822 2.1517C16.3622 2.22339 16.413 2.322 16.425 2.42875L16.6562 4.21875C16.6644 4.28168 16.6591 4.34561 16.6408 4.40636C16.6225 4.46711 16.5916 4.52331 16.55 4.57125C16.5098 4.61897 16.4597 4.65734 16.4031 4.68368C16.3465 4.71002 16.2849 4.72369 16.2225 4.72375H14.6325C14.5744 4.7231 14.517 4.711 14.4635 4.68816C14.4101 4.66532 14.3617 4.63217 14.3211 4.59062C14.2804 4.54906 14.2484 4.49991 14.2268 4.44597C14.2051 4.39203 14.1943 4.33436 14.195 4.27625V2.4875ZM8.945 2.4875C8.945 2.24 9.14125 2.04 9.3825 2.04H12.8825C13.1237 2.04 13.32 2.24 13.32 2.4875V4.275C13.3207 4.33311 13.3099 4.39078 13.2882 4.44472C13.2666 4.49866 13.2346 4.54781 13.1939 4.58937C13.1533 4.63092 13.1049 4.66407 13.0515 4.68691C12.998 4.70975 12.9406 4.72185 12.8825 4.7225H9.3825C9.32439 4.72185 9.26698 4.70975 9.21354 4.68691C9.1601 4.66407 9.11169 4.63092 9.07106 4.58937C9.03044 4.54781 8.99839 4.49866 8.97676 4.44472C8.95513 4.39078 8.94434 4.33311 8.945 4.275V2.4875ZM3.695 2.4875C3.695 2.24 3.89125 2.04 4.1325 2.04H7.6325C7.875 2.04 8.07125 2.24 8.07125 2.4875V4.275C8.07225 4.39226 8.0268 4.50516 7.94482 4.58901C7.86284 4.67286 7.75101 4.72085 7.63375 4.7225H4.13375C4.07564 4.72185 4.01823 4.70975 3.96479 4.68691C3.91135 4.66407 3.86294 4.63092 3.82231 4.58937C3.78169 4.54781 3.74964 4.49866 3.72801 4.44472C3.70638 4.39078 3.69559 4.33311 3.69625 4.275V2.4875H3.695ZM1.07 2.4875C1.07 2.24 1.2675 2.04 1.5075 2.04H2.38375C2.625 2.04 2.82 2.24 2.82 2.4875V4.275C2.82083 4.33305 2.81019 4.39069 2.7887 4.44463C2.7672 4.49856 2.73528 4.54772 2.69475 4.58929C2.65423 4.63086 2.60589 4.66403 2.55253 4.68689C2.49916 4.70975 2.4418 4.72185 2.38375 4.7225H1.50875C1.45064 4.72185 1.39323 4.70975 1.33979 4.68691C1.28635 4.66407 1.23794 4.63092 1.19731 4.58937C1.15669 4.54781 1.12464 4.49866 1.10301 4.44472C1.08138 4.39078 1.07059 4.33311 1.07125 4.275V2.4875H1.07Z"
            , SA.fill "black"
            ]
            []
        , S.path
            [ SA.d "M12.1249 8.75003C12.1177 8.58133 12.1446 8.4129 12.2042 8.25491C12.2638 8.09692 12.3548 7.95264 12.4717 7.83078C12.5885 7.70892 12.7289 7.612 12.8843 7.54588C13.0396 7.47975 13.2068 7.44579 13.3757 7.44605C13.5445 7.4463 13.7116 7.48076 13.8667 7.54735C14.0219 7.61394 14.162 7.71128 14.2785 7.83349C14.395 7.95571 14.4856 8.10026 14.5447 8.25843C14.6038 8.4166 14.6302 8.5851 14.6224 8.75378C14.6076 9.07528 14.4693 9.37867 14.2363 9.60076C14.0034 9.82285 13.6938 9.94653 13.3719 9.94604C13.0501 9.94556 12.7408 9.82095 12.5085 9.59816C12.2762 9.37537 12.1388 9.07158 12.1249 8.75003Z"
            , SA.fill "black"
            ]
            []
        , S.path
            [ SA.d "M4.68756 7.49976C3.99881 7.49976 3.43756 8.06101 3.43756 8.74976C3.43756 9.08128 3.56926 9.39922 3.80368 9.63364C4.0381 9.86806 4.35604 9.99976 4.68756 9.99976C5.01908 9.99976 5.33702 9.86806 5.57144 9.63364C5.80586 9.39922 5.93756 9.08128 5.93756 8.74976C5.93756 8.06101 5.37631 7.49976 4.68756 7.49976Z"
            , SA.fill "black"
            ]
            []
        ]


info : Html msg
info =
    S.svg [ SA.width "40", SA.height "40", SA.viewBox "0 0 40 40", SA.fill "none" ]
        [ S.path [ SA.d "M22 12V16H18V12H22Z", SA.fill "black" ] []
        , S.path [ SA.d "M18 18V28H22V18H18Z", SA.fill "black" ] []
        , S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M2 20C2 10.0589 10.0589 2 20 2C29.9411 2 38 10.0589 38 20C38 29.9411 29.9411 38 20 38C10.0589 38 2 29.9411 2 20ZM20 6C12.268 6 6 12.268 6 20C6 27.732 12.268 34 20 34C27.732 34 34 27.732 34 20C34 12.268 27.732 6 20 6Z"
            , SA.fill "black"
            ]
            []
        ]


warning : Html msg
warning =
    S.svg [ SA.width "20", SA.height "20", SA.viewBox "0 0 20 20", SA.fill "none" ]
        [ S.path [ SA.d "M9 6V11H11V6H9Z", SA.fill "black" ] []
        , S.path [ SA.d "M11 12H9V14H11V12Z", SA.fill "black" ] []
        , S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M7 1C6.73478 1 6.48043 1.10536 6.29289 1.29289L1.29289 6.29289C1.10536 6.48043 1 6.73478 1 7V13C1 13.2652 1.10536 13.5196 1.29289 13.7071L6.29289 18.7071C6.48043 18.8946 6.73478 19 7 19H13C13.2652 19 13.5196 18.8946 13.7071 18.7071L18.7071 13.7071C18.8946 13.5196 19 13.2652 19 13V7C19 6.73478 18.8946 6.48043 18.7071 6.29289L13.7071 1.29289C13.5196 1.10536 13.2652 1 13 1H7ZM3 7.41421L7.41421 3H12.5858L17 7.41421V12.5858L12.5858 17H7.41421L3 12.5858V7.41421Z"
            , SA.fill "black"
            ]
            []
        ]


error : Html msg
error =
    S.svg [ SA.width "18", SA.height "16", SA.viewBox "0 0 18 16", SA.fill "none" ]
        [ S.path [ SA.d "M10 11V13H8V11H10Z", SA.fill "white" ] []
        , S.path [ SA.d "M8 6V10H10V6H8Z", SA.fill "white" ] []
        , S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M9 0C9.35886 0 9.6902 0.192286 9.86824 0.503861L17.8682 14.5039C18.0451 14.8134 18.0438 15.1936 17.8649 15.5019C17.686 15.8102 17.3565 16 17 16H1C0.64353 16 0.314012 15.8102 0.13509 15.5019C-0.043832 15.1936 -0.045102 14.8134 0.131757 14.5039L8.13176 0.503861C8.3098 0.192286 8.64114 0 9 0ZM15.2768 14L9 3.01556L2.72318 14H15.2768Z"
            , SA.fill "white"
            ]
            []
        ]


edit : Html msg
edit =
    S.svg [ SA.width "21", SA.height "20", SA.viewBox "0 0 21 20", SA.fill "none" ]
        [ S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M13.6264 2.29289C14.0169 1.90237 14.6501 1.90237 15.0406 2.29289L18.0406 5.29289C18.4311 5.68342 18.4311 6.31658 18.0406 6.70711L7.0406 17.7071C6.85307 17.8946 6.59871 18 6.3335 18H3.3335C2.78121 18 2.3335 17.5523 2.3335 17V14C2.3335 13.7348 2.43885 13.4804 2.62639 13.2929L13.6264 2.29289ZM12.7477 6L14.3335 7.58579L15.9193 6L14.3335 4.41421L12.7477 6ZM12.9193 9L11.3335 7.41421L5.75617 12.9915C6.42374 13.3582 6.97534 13.9098 7.34196 14.5773L12.9193 9ZM5.77049 16C5.5896 15.2972 5.0363 14.7439 4.3335 14.563V16H5.77049Z"
            , SA.fill "black"
            ]
            []
        ]


delete : Html msg
delete =
    S.svg [ SA.width "16", SA.height "18", SA.viewBox "0 0 16 18", SA.fill "none" ]
        [ S.path
            [ SA.d "M5.5 14V7H7.5V14H5.5Z"
            , SA.fill "black"
            ]
            []
        , S.path
            [ SA.d "M8.5 7V14H10.5V7H8.5Z"
            , SA.fill "black"
            ]
            []
        , S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M5 1C5 0.447715 5.44772 0 6 0H10C10.5523 0 11 0.447715 11 1V3H16V5H14.5V17C14.5 17.5523 14.0523 18 13.5 18H2.5C1.94772 18 1.5 17.5523 1.5 17V5H0V3H5V1ZM7 3H9V2H7V3ZM3.5 5V16H12.5V5H3.5Z"
            , SA.fill "black"
            ]
            []
        ]


creditcard : Html msg
creditcard =
    S.svg [ SA.width "18", SA.height "14", SA.viewBox "0 0 18 14", SA.fill "none" ]
        [ S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M0 3C0 1.34315 1.34315 0 3 0H15C16.6569 0 18 1.34315 18 3V11C18 12.6569 16.6569 14 15 14H3C1.34315 14 0 12.6569 0 11V3ZM3 2C2.44772 2 2 2.44772 2 3V6H16V3C16 2.44772 15.5523 2 15 2H3ZM16 10H2V11C2 11.5523 2.44772 12 3 12H15C15.5523 12 16 11.5523 16 11V10Z"
            , SA.fill "white"
            ]
            []
        ]


{-| Vipps icon - should be embedded in a box with a background color of #FF5B24.
-}
vipps : Html msg
vipps =
    S.svg [ SA.width "20", SA.height "20", SA.viewBox "0 0 20 20", SA.fill "none" ]
        [ S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M17.6875 4H2.3125C1.58763 4 1 4.58763 1 5.3125V14.6875C1 15.4124 1.58763 16 2.3125 16H17.6875C18.4124 16 19 15.4124 19 14.6875V5.3125C19 4.58763 18.4124 4 17.6875 4Z"
            , SA.fill "#FF5B24"
            ]
            []
        , S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M11.449 8.59844C11.9654 8.59844 12.4079 8.20641 12.4079 7.64294H12.408C12.408 7.07933 11.9654 6.68744 11.449 6.68744C10.9327 6.68744 10.4903 7.07933 10.4903 7.64294C10.4903 8.20641 10.9327 8.59844 11.449 8.59844ZM12.7029 10.1423C12.0635 10.9752 11.3874 11.551 10.1949 11.551H10.1951C8.97843 11.551 8.03166 10.8159 7.29415 9.73791C6.99902 9.2968 6.54419 9.19885 6.21224 9.43161C5.90496 9.65219 5.83139 10.1177 6.11395 10.5221C7.13417 12.078 8.54777 12.9844 10.1949 12.9844C11.7071 12.9844 12.8873 12.2494 13.8091 11.0243C14.1532 10.5711 14.1409 10.1055 13.8091 9.8482C13.5017 9.60295 13.0469 9.68909 12.7029 10.1423V10.1423Z"
            , SA.fill "white"
            ]
            []
        ]


cross : Html msg
cross =
    S.svg [ SA.width "14", SA.height "14", SA.viewBox "0 0 14 14", SA.fill "none" ]
        [ S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M7.00007 8.41429L12.293 13.7072L13.7072 12.293L8.41429 7.00007L13.7072 1.70718L12.293 0.292969L7.00007 5.58586L1.70718 0.292969L0.292969 1.70718L5.58586 7.00007L0.292969 12.293L1.70718 13.7072L7.00007 8.41429Z"
            , SA.fill "black"
            ]
            []
        ]


{-| Single ticket.
-}
ticket : Html msg
ticket =
    S.svg [ SA.width "20", SA.height "20", SA.viewBox "0 0 20 20", SA.fill "none" ]
        [ S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M1 4C1 3.44772 1.44772 3 2 3H18C18.5523 3 19 3.44772 19 4V8C19 8.54879 18.5579 8.99433 18.0105 8.99995C18.0073 9.00006 17.9986 9.00041 17.9853 9.00137C17.9572 9.00337 17.9105 9.00787 17.8519 9.01764C17.7299 9.03798 17.5825 9.07678 17.4472 9.14443C17.3158 9.21011 17.2149 9.29294 17.1446 9.39845C17.0784 9.4977 17 9.67498 17 10C17 10.325 17.0784 10.5023 17.1446 10.6015C17.2149 10.7071 17.3158 10.7899 17.4472 10.8556C17.5825 10.9232 17.7299 10.962 17.8519 10.9824C17.9105 10.9921 17.9572 10.9966 17.9853 10.9986C17.9986 10.9996 18.0073 10.9999 18.0105 11.0001C18.5579 11.0057 19 11.4512 19 12V16C19 16.5523 18.5523 17 18 17H2C1.44772 17 1 16.5523 1 16V12C1 11.4512 1.44207 11.0057 1.98953 11.0001C1.99274 10.9999 2.00138 10.9996 2.01469 10.9986C2.04279 10.9966 2.08947 10.9921 2.1481 10.9824C2.27011 10.962 2.4175 10.9232 2.55279 10.8556C2.68416 10.7899 2.78511 10.7071 2.85545 10.6015C2.92162 10.5023 3 10.325 3 10C3 9.67498 2.92162 9.4977 2.85545 9.39845C2.78511 9.29294 2.68416 9.21011 2.55279 9.14443C2.4175 9.07678 2.27011 9.03798 2.1481 9.01764C2.08947 9.00787 2.04279 9.00337 2.01469 9.00137C2.00138 9.00041 1.99274 9.00006 1.98953 8.99995C1.44207 8.99433 1 8.54879 1 8V4ZM3 7.17265C3.14259 7.2192 3.29383 7.27888 3.44721 7.35557C3.81584 7.53989 4.21489 7.83206 4.51955 8.28905C4.82838 8.7523 5 9.32502 5 10C5 10.675 4.82838 11.2477 4.51955 11.711C4.21489 12.1679 3.81584 12.4601 3.44721 12.6444C3.29383 12.7211 3.14259 12.7808 3 12.8274V15H17V12.8274C16.8574 12.7808 16.7062 12.7211 16.5528 12.6444C16.1842 12.4601 15.7851 12.1679 15.4804 11.711C15.1716 11.2477 15 10.675 15 10C15 9.32502 15.1716 8.7523 15.4804 8.28905C15.7851 7.83206 16.1842 7.53989 16.5528 7.35557C16.7062 7.27888 16.8574 7.2192 17 7.17265V5H3V7.17265Z"
            , SA.fill "black"
            ]
            []
        ]


{-| Single ticket with a "+" in the middle.
-}
ticketAdd : Html msg
ticketAdd =
    S.svg [ SA.width "18", SA.height "14", SA.viewBox "0 0 18 14", SA.fill "none" ]
        [ S.path
            [ SA.d "M10 4H8V6H6V8H8V10H10V8H12V6H10V4Z"
            , SA.fill "black"
            ]
            []
        , S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M1 0C0.447715 0 0 0.447715 0 1V5C0 5.54879 0.442068 5.99433 0.989531 5.99995C0.992739 6.00006 1.00138 6.00041 1.01469 6.00137C1.04279 6.00337 1.08947 6.00787 1.1481 6.01764C1.27011 6.03798 1.4175 6.07678 1.55279 6.14443C1.68416 6.21011 1.78511 6.29294 1.85545 6.39845C1.92162 6.4977 2 6.67498 2 7C2 7.32502 1.92162 7.5023 1.85545 7.60155C1.78511 7.70706 1.68416 7.78989 1.55279 7.85557C1.4175 7.92322 1.27011 7.96202 1.1481 7.98236C1.08947 7.99213 1.04279 7.99663 1.01469 7.99863C1.00138 7.99959 0.992739 7.99994 0.989531 8.00005C0.442068 8.00567 0 8.45121 0 9V13C0 13.5523 0.447715 14 1 14H17C17.5523 14 18 13.5523 18 13V9C18 8.45121 17.5579 8.00567 17.0105 8.00005C17.0073 7.99994 16.9986 7.99959 16.9853 7.99863C16.9572 7.99663 16.9105 7.99213 16.8519 7.98236C16.7299 7.96202 16.5825 7.92322 16.4472 7.85557C16.3158 7.78989 16.2149 7.70706 16.1446 7.60155C16.0784 7.5023 16 7.32502 16 7C16 6.67498 16.0784 6.4977 16.1446 6.39845C16.2149 6.29294 16.3158 6.21011 16.4472 6.14443C16.5825 6.07678 16.7299 6.03798 16.8519 6.01764C16.9105 6.00787 16.9572 6.00337 16.9853 6.00137C16.9986 6.00041 17.0073 6.00006 17.0105 5.99995C17.5579 5.99433 18 5.54879 18 5V1C18 0.447715 17.5523 0 17 0H1ZM2.44721 4.35557C2.29383 4.27888 2.14259 4.2192 2 4.17265V2H16V4.17265C15.8574 4.2192 15.7062 4.27888 15.5528 4.35557C15.1842 4.53989 14.7851 4.83206 14.4804 5.28905C14.1716 5.7523 14 6.32502 14 7C14 7.67498 14.1716 8.24771 14.4804 8.71095C14.7851 9.16794 15.1842 9.46011 15.5528 9.64443C15.7062 9.72112 15.8574 9.7808 16 9.82735V12H2V9.82735C2.14259 9.7808 2.29383 9.72112 2.44721 9.64443C2.81584 9.46011 3.21489 9.16794 3.51955 8.71095C3.82838 8.24771 4 7.67498 4 7C4 6.32502 3.82838 5.7523 3.51955 5.28905C3.21489 4.83206 2.81584 4.53989 2.44721 4.35557Z"
            , SA.fill "black"
            ]
            []
        ]


{-| Single ticket with an "x" in the middle.
-}
ticketRemove : Html msg
ticketRemove =
    S.svg [ SA.width "18", SA.height "14", SA.viewBox "0 0 18 14", SA.fill "none" ]
        [ S.path
            [ SA.d "M12.2071 5.20711L10.4142 7L12.2071 8.79289L10.7929 10.2071L9 8.41421L7.20711 10.2071L5.79289 8.79289L7.58579 7L5.79289 5.20711L7.20711 3.79289L9 5.58579L10.7929 3.79289L12.2071 5.20711Z"
            , SA.fill "black"
            ]
            []
        , S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M1 0C0.447715 0 0 0.447715 0 1V5C0 5.54879 0.442068 5.99433 0.989531 5.99995C0.992739 6.00006 1.00138 6.00041 1.01469 6.00137C1.04279 6.00337 1.08947 6.00787 1.1481 6.01764C1.27011 6.03798 1.4175 6.07678 1.55279 6.14443C1.68416 6.21011 1.78511 6.29294 1.85545 6.39845C1.92162 6.4977 2 6.67498 2 7C2 7.32502 1.92162 7.5023 1.85545 7.60155C1.78511 7.70706 1.68416 7.78989 1.55279 7.85557C1.4175 7.92322 1.27011 7.96202 1.1481 7.98236C1.08947 7.99213 1.04279 7.99663 1.01469 7.99863C1.00138 7.99959 0.992739 7.99994 0.989531 8.00005C0.442068 8.00567 0 8.45121 0 9V13C0 13.5523 0.447715 14 1 14H17C17.5523 14 18 13.5523 18 13V9C18 8.45121 17.5579 8.00567 17.0105 8.00005C17.0073 7.99994 16.9986 7.99959 16.9853 7.99863C16.9572 7.99663 16.9105 7.99213 16.8519 7.98236C16.7299 7.96202 16.5825 7.92322 16.4472 7.85557C16.3158 7.78989 16.2149 7.70706 16.1446 7.60155C16.0784 7.5023 16 7.32502 16 7C16 6.67498 16.0784 6.4977 16.1446 6.39845C16.2149 6.29294 16.3158 6.21011 16.4472 6.14443C16.5825 6.07678 16.7299 6.03798 16.8519 6.01764C16.9105 6.00787 16.9572 6.00337 16.9853 6.00137C16.9986 6.00041 17.0073 6.00006 17.0105 5.99995C17.5579 5.99433 18 5.54879 18 5V1C18 0.447715 17.5523 0 17 0H1ZM2.44721 4.35557C2.29383 4.27888 2.14259 4.2192 2 4.17265V2H16V4.17265C15.8574 4.2192 15.7062 4.27888 15.5528 4.35557C15.1842 4.53989 14.7851 4.83206 14.4804 5.28905C14.1716 5.7523 14 6.32502 14 7C14 7.67498 14.1716 8.24771 14.4804 8.71095C14.7851 9.16794 15.1842 9.46011 15.5528 9.64443C15.7062 9.72112 15.8574 9.7808 16 9.82735V12H2V9.82735C2.14259 9.7808 2.29383 9.72112 2.44721 9.64443C2.81584 9.46011 3.21489 9.16794 3.51955 8.71095C3.82838 8.24771 4 7.67498 4 7C4 6.32502 3.82838 5.7523 3.51955 5.28905C3.21489 4.83206 2.81584 4.53989 2.44721 4.35557Z"
            , SA.fill "black"
            ]
            []
        ]


{-| Multiple tickets stacked on top of each other.
-}
tickets : Html msg
tickets =
    S.svg [ SA.width "17", SA.height "14", SA.viewBox "0 0 17 14", SA.fill "none" ]
        [ S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M12.5664 5.81249C12.5471 5.78355 12.5 5.70002 12.5 5.5C12.5 5.29998 12.5471 5.21645 12.5664 5.18751C12.5899 5.15231 12.6283 5.11636 12.6972 5.08193C12.77 5.04553 12.8549 5.02235 12.93 5.00983C12.9652 5.00397 12.9924 5.00142 13.0068 5.00039L13.0147 4.99989C13.5602 4.99203 14 4.54737 14 4V1C14 0.447715 13.5523 0 13 0H1C0.447715 0 0 0.447715 0 1V4C0 4.54737 0.439786 4.99203 0.985289 4.99989L0.993206 5.00039C1.00764 5.00142 1.03478 5.00397 1.06998 5.00983C1.14511 5.02235 1.23 5.04553 1.30279 5.08193C1.37166 5.11636 1.41011 5.15231 1.43357 5.18751C1.45287 5.21645 1.5 5.29998 1.5 5.5C1.5 5.70002 1.45287 5.78355 1.43357 5.81249C1.41011 5.84769 1.37166 5.88364 1.30279 5.91807C1.23 5.95447 1.14511 5.97765 1.06998 5.99017C1.03478 5.99603 1.00764 5.99858 0.993206 5.99961L0.985289 6.00011C0.439786 6.00797 0 6.45263 0 7V10C0 10.5523 0.447715 11 1 11H13C13.5523 11 14 10.5523 14 10V7C14 6.45263 13.5602 6.00797 13.0147 6.00011L13.0068 5.99961C12.9924 5.99858 12.9652 5.99603 12.93 5.99017C12.8549 5.97765 12.77 5.95447 12.6972 5.91807C12.6283 5.88364 12.5899 5.84769 12.5664 5.81249ZM2 3.20445V2H12V3.20445C11.9354 3.2304 11.8694 3.25978 11.8028 3.29307C11.4967 3.44614 11.1601 3.69144 10.9023 4.07811C10.6404 4.47105 10.5 4.95002 10.5 5.5C10.5 6.04998 10.6404 6.52895 10.9023 6.92189C11.1601 7.30856 11.4967 7.55386 11.8028 7.70693C11.8694 7.74022 11.9354 7.7696 12 7.79555V9H2V7.79555C2.06464 7.7696 2.13063 7.74022 2.19721 7.70693C2.50334 7.55386 2.83989 7.30856 3.09768 6.92189C3.35963 6.52895 3.5 6.04998 3.5 5.5C3.5 4.95002 3.35963 4.47105 3.09768 4.07811C2.83989 3.69144 2.50334 3.44614 2.19721 3.29307C2.13063 3.25978 2.06464 3.2304 2 3.20445Z"
            , SA.fill "black"
            ]
            []
        , S.path
            [ SA.d "M15 3V12H2V14H16C16.5523 14 17 13.5523 17 13V3H15Z"
            , SA.fill "black"
            ]
            []
        ]


{-| Up arrow. Styled for expanding and collapsing.
-}
upArrow : Html msg
upArrow =
    S.svg [ SA.width "16", SA.height "9", SA.viewBox "0 0 16 9", SA.fill "none" ]
        [ S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M7.29297 0.292893C7.68349 -0.0976311 8.31666 -0.0976311 8.70718 0.292893L15.7072 7.29289L14.293 8.70711L8.00008 2.41421L1.70718 8.70711L0.292969 7.29289L7.29297 0.292893Z"
            , SA.fill "black"
            ]
            []
        ]


{-| Down arrow. Styled for expanding and collapsing.
-}
downArrow : Html msg
downArrow =
    S.svg [ SA.width "17", SA.height "9", SA.viewBox "0 0 17 9", SA.fill "none" ]
        [ S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M7.79297 8.70711C8.18349 9.09763 8.81666 9.09763 9.20718 8.70711L16.2072 1.70711L14.793 0.292893L8.50008 6.58579L2.20718 0.292893L0.792969 1.70711L7.79297 8.70711Z"
            , SA.fill "black"
            ]
            []
        ]


{-| Left arrow. Styled for navigating back.
-}
leftArrow : Html msg
leftArrow =
    S.svg [ SA.width "16", SA.height "12", SA.viewBox "0 0 16 12", SA.fill "none" ]
        [ S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M0.292893 5.29297L5.29289 0.292969L6.70711 1.70718L3.41421 5.00008H16V7.00008H3.41421L6.70711 10.293L5.29289 11.7072L0.292893 6.70718C-0.0976311 6.31666 -0.0976311 5.68349 0.292893 5.29297Z"
            , SA.fill "black"
            ]
            []
        ]


leftCaret : Html msg
leftCaret =
    S.svg [ SA.width "6", SA.height "10", SA.viewBox "0 0 6 10", SA.fill "none" ]
        [ S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M0.292893 4.29297L4.29289 0.292969L5.70711 1.70718L2.41421 5.00008L5.70711 8.29297L4.29289 9.70718L0.292893 5.70718C-0.0976311 5.31666 -0.0976311 4.68349 0.292893 4.29297Z"
            , SA.fill "black"
            ]
            []
        ]


rightCaret : Html msg
rightCaret =
    S.svg [ SA.width "6", SA.height "10", SA.viewBox "0 0 6 10", SA.fill "none" ]
        [ S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M3.58586 5.00008L0.292969 1.70718L1.70718 0.292969L5.70718 4.29297C6.09771 4.68349 6.09771 5.31666 5.70718 5.70718L1.70718 9.70718L0.292969 8.29297L3.58586 5.00008Z"
            , SA.fill "black"
            ]
            []
        ]


{-| Right arrow. Styled for navigating forward.
-}
rightArrow : Html msg
rightArrow =
    S.svg [ SA.width "20", SA.height "20", SA.viewBox "0 0 20 20", SA.fill "none" ]
        [ S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M14.5858 9.00008L11.2929 5.70718L12.7071 4.29297L17.7071 9.29297C18.0976 9.68349 18.0976 10.3167 17.7071 10.7072L12.7071 15.7072L11.2929 14.293L14.5858 11.0001H2V9.00008H14.5858Z"
            , SA.fill "black"
            ]
            []
        ]


chat : Html msg
chat =
    S.svg [ SA.width "16", SA.height "16", SA.viewBox "0 0 16 16", SA.fill "none" ]
        [ S.path
            [ SA.d "M0 3.5C0 1.84315 1.34315 0.5 3 0.5H13C14.6569 0.5 16 1.84315 16 3.5V9.5C16 11.1569 14.6569 12.5 13 12.5H11.4142L8 15.9142V12.5H3C1.34315 12.5 0 11.1569 0 9.5V3.5Z"
            , SA.fill "black"
            ]
            []
        ]


traveler : Html msg
traveler =
    S.svg [ SA.width "16", SA.height "18", SA.viewBox "0 0 16 18", SA.fill "none" ]
        [ S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M8 0C5.79086 0 4 1.79086 4 4C4 6.20914 5.79086 8 8 8C10.2091 8 12 6.20914 12 4C12 1.79086 10.2091 0 8 0ZM6 4C6 2.89543 6.89543 2 8 2C9.10457 2 10 2.89543 10 4C10 5.10457 9.10457 6 8 6C6.89543 6 6 5.10457 6 4Z"
            , SA.fill "black"
            ]
            []
        , S.path
            [ SA.d "M2 17C2 13.6863 4.68629 11 8 11C11.3137 11 14 13.6863 14 17V18H16V17C16 12.5817 12.4183 9 8 9C3.58172 9 0 12.5817 0 17V18H2V17Z"
            , SA.fill "black"
            ]
            []
        ]


duration : Html msg
duration =
    S.svg [ SA.width "18", SA.height "18", SA.viewBox "0 0 18 18", SA.fill "none" ]
        [ S.path
            [ SA.d "M9 4C10.2781 4 11.5594 4.48835 12.5355 5.46446C14.4882 7.41708 14.4882 10.5829 12.5355 12.5355C12.145 12.9261 11.5118 12.9261 11.1213 12.5355L8.29289 9.70711C8.10536 9.51957 8 9.26521 8 9V5C8 4.44771 8.44772 4 9 4Z"
            , SA.fill "black"
            ]
            []
        , S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M9 0C4.02944 0 0 4.02944 0 9C0 13.9706 4.02944 18 9 18C13.9706 18 18 13.9706 18 9C18 4.02944 13.9706 0 9 0ZM2 9C2 5.13401 5.13401 2 9 2C12.866 2 16 5.13401 16 9C16 12.866 12.866 16 9 16C5.13401 16 2 12.866 2 9Z"
            , SA.fill "black"
            ]
            []
        ]


toggleOff : Html msg
toggleOff =
    S.svg [ SA.width "36", SA.height "20", SA.viewBox "0 0 36 20", SA.fill "none" ]
        [ S.rect [ SA.width "36", SA.height "20", SA.rx "10", SA.fill "#878e92" ] []
        , S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M10 18C14.4183 18 18 14.4183 18 10C18 5.58172 14.4183 2 10 2C5.58172 2 2 5.58172 2 10C2 14.4183 5.58172 18 10 18Z"
            , SA.fill "white"
            ]
            []
        ]


toggleOn : Html msg
toggleOn =
    S.svg [ SA.width "36", SA.height "20", SA.viewBox "0 0 36 20", SA.fill "none" ]
        [ S.rect [ SA.width "36", SA.height "20", SA.rx "10", SA.fill "#a2ad00" ] []
        , S.path
            [ SA.fillRule "evenodd"
            , SA.clipRule "evenodd"
            , SA.d "M26 18C30.4183 18 34 14.4183 34 10C34 5.58172 30.4183 2 26 2C21.5817 2 18 5.58172 18 10C18 14.4183 21.5817 18 26 18Z"
            , SA.fill "white"
            ]
            []
        ]


logout : Html msg
logout =
    S.svg [ SA.width "17", SA.height "16", SA.viewBox "0 0 16 16", SA.fill "none" ]
        [ S.path
            [ SA.d "M6 3C6 1.34315 7.34315 0 9 0H14C15.6569 0 17 1.34315 17 3V13C17 14.6569 15.6569 16 14 16H9C7.34315 16 6 14.6569 6 13V11H8V13C8 13.5523 8.44772 14 9 14H14C14.5523 14 15 13.5523 15 13V3C15 2.44772 14.5523 2 14 2H9C8.44772 2 8 2.44772 8 3V5H6V3Z"
            , SA.fill "black"
            ]
            []
        , S.path
            [ SA.d "M3.79289 3.79289L0.292893 7.29289C-0.0976311 7.68342 -0.0976311 8.31658 0.292893 8.70711L3.79289 12.2071L5.20711 10.7929L3.41421 9H11.5V7H3.41421L5.20711 5.20711L3.79289 3.79289Z"
            , SA.fill "black"
            ]
            []
        ]
