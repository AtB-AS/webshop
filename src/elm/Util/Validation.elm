module Util.Validation exposing
    ( FormError
    , ValidationErrors
    , addValidationError
    , clearValidationError
    , emailValidator
    , nameValidator
    , selectValidationError
    , travelCardValidator
    , validate
    )

import List.Extra
import Validate exposing (Valid, Validator)


type alias FormError a =
    ( a, String )


type alias ValidationErrors a =
    List (FormError a)


selectValidationError : a -> ValidationErrors a -> Maybe String
selectValidationError fieldName =
    List.Extra.find (Tuple.first >> (==) fieldName) >> Maybe.map Tuple.second


clearValidationError : a -> ValidationErrors a -> ValidationErrors a
clearValidationError fieldName =
    List.filter (Tuple.first >> (/=) fieldName)


addValidationError : List a -> String -> ValidationErrors a -> ValidationErrors a
addValidationError fields error =
    (++) (fields |> List.map (\a -> ( a, error )))


travelCardValidator : a -> (subject -> String) -> Validate.Validator (FormError a) subject
travelCardValidator field toValue =
    Validate.firstError
        [ Validate.ifBlank toValue ( field, "t:kort id kan ikke være tomt." )
        , ifNotLength 9 toValue ( field, "t:kort id ser ut til å være feil." )
        , Validate.ifNotInt toValue (\_ -> ( field, "t:kort id må være et tall." ))
        ]


emailValidator : a -> (subject -> String) -> Validate.Validator (FormError a) subject
emailValidator field toValue =
    Validate.firstError
        [ Validate.ifInvalidEmail toValue (\_ -> ( field, "E-posten du har skrevet ser ikke ut til å være gyldig" ))
        ]


nameValidator : a -> (subject -> String) -> Validate.Validator (FormError a) subject
nameValidator field toValue =
    Validate.firstError
        [ Validate.ifBlank toValue ( field, "E-posten du har skrevet ser ikke ut til å være gyldig" )
        ]


validate : Validator error subject -> subject -> Result (List error) (Valid subject)
validate =
    Validate.validate



-- Internals


ifNotLength : Int -> (subject -> String) -> error -> Validate.Validator error subject
ifNotLength stringLength subjectToString error =
    Validate.ifTrue (\subject -> String.length (subjectToString subject) /= stringLength) error
