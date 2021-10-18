module Util.Validation exposing
    ( FormError
    , ValidationErrors
    , add
    , all
    , emailValidator
    , init
    , passwordValidator
    , phoneValidator
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


travelCardValidator : String -> a -> (subject -> String) -> Validate.Validator (FormError a) subject
travelCardValidator travelCardPrefix field toValue =
    Validate.firstError
        [ Validate.ifBlank toValue ( field, "Reisekortnummeret id kan ikke være tomt." )
        , ifNotLength 16 toValue ( field, "Reisekortnummeret ser ut til å være feil." )
        , Validate.ifNotInt toValue (\_ -> ( field, "Reisekortnummeret må være et tall på 16 siffer." ))
        , Validate.ifFalse (\model -> String.startsWith travelCardPrefix (toValue model)) ( field, "Reisekortnummeret må starte på " ++ travelCardPrefix )
        ]


phoneValidator : a -> (subject -> String) -> Validate.Validator (FormError a) subject
phoneValidator field toValue =
    Validate.firstError
        [ Validate.ifBlank toValue ( field, "Telefonnummeret kan ikke være tomt." )
        , Validate.ifFalse (\model -> String.startsWith "+" (toValue model)) ( field, "Telefonnummeret må inkludere landskode" )
        , Validate.ifNotInt (toValue >> String.replace "+" "") (\_ -> ( field, "Telefonnummeret kan kun bestå av siffer (med eventuell landskode)." ))
        ]


emailValidator : a -> (subject -> String) -> Validate.Validator (FormError a) subject
emailValidator field toValue =
    Validate.firstError
        [ Validate.ifBlank toValue ( field, "E-postadresse kan ikke være tomt." )
        , Validate.ifInvalidEmail toValue (\_ -> ( field, "E-posten du har skrevet ser ikke ut til å være gyldig" ))
        ]


passwordValidator : a -> (subject -> String) -> Validate.Validator (FormError a) subject
passwordValidator field toValue =
    Validate.firstError
        [ Validate.ifBlank toValue ( field, "Passord kan ikke være tomt." )
        , Validate.ifFalse (\i -> String.length (toValue i) >= 6) ( field, "Passordet må være over 6 tegn" )
        ]


void : Validate.Validator (FormError a) subject
void =
    Validate.firstError
        []


validate : Validator error subject -> subject -> Result (List error) (Valid subject)
validate =
    Validate.validate


all : List (Validator error subject) -> Validator error subject
all =
    Validate.all



-- Internals


ifNotLength : Int -> (subject -> String) -> error -> Validate.Validator error subject
ifNotLength stringLength subjectToString error =
    Validate.ifTrue (\subject -> String.length (subjectToString subject) /= stringLength) error
