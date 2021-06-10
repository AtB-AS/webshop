module Util.Validation exposing
    ( FormError
    , ValidationErrors
    , add
    , emailValidator
    , init
    , remove
    , removeAll
    , select
    , travelCardValidator
    , validate
    , void
    )

import List.Extra
import Validate exposing (Valid, Validator)


type alias FormError a =
    ( a, String )


type alias ValidationErrors a =
    List (FormError a)


select : a -> ValidationErrors a -> Maybe String
select fieldName =
    List.Extra.find (Tuple.first >> (==) fieldName) >> Maybe.map Tuple.second


remove : a -> ValidationErrors a -> ValidationErrors a
remove fieldName =
    List.filter (Tuple.first >> (/=) fieldName)


removeAll : List a -> ValidationErrors a -> ValidationErrors a
removeAll fieldNames =
    List.filter (\item -> not <| List.member (Tuple.first item) fieldNames)


init : ValidationErrors a
init =
    []


add : List a -> String -> ValidationErrors a -> ValidationErrors a
add fields error =
    (++) (fields |> List.map (\a -> ( a, error )))


travelCardValidator : a -> (subject -> String) -> Validate.Validator (FormError a) subject
travelCardValidator field toValue =
    Validate.firstError
        [ Validate.ifBlank toValue ( field, "t:kort id kan ikke være tomt." )
        , ifNotLength 16 toValue ( field, "t:kort id ser ut til å være feil." )
        , Validate.ifNotInt toValue (\_ -> ( field, "t:kort id må være et tall på 16 siffer." ))
        ]


emailValidator : a -> (subject -> String) -> Validate.Validator (FormError a) subject
emailValidator field toValue =
    Validate.firstError
        [ Validate.ifBlank toValue ( field, "E-post kan ikke være tomt." )
        , Validate.ifInvalidEmail toValue (\_ -> ( field, "E-posten du har skrevet ser ikke ut til å være gyldig" ))
        ]


void : Validate.Validator (FormError a) subject
void =
    Validate.firstError
        []


validate : Validator error subject -> subject -> Result (List error) (Valid subject)
validate =
    Validate.validate



-- Internals


ifNotLength : Int -> (subject -> String) -> error -> Validate.Validator error subject
ifNotLength stringLength subjectToString error =
    Validate.ifTrue (\subject -> String.length (subjectToString subject) /= stringLength) error
