module NHSNumber exposing (show
                          , validateFromString
                          , validateFromList)

{-| NHSNumber validation utilities

Validation follows the NHS data dictionary requirements:

http://www.datadictionary.nhs.uk/data_dictionary/attributes/n/nhs/nhs_number_de.asp

> The NHS NUMBER is 10 numeric digits in length. The tenth digit is a check digit used to confirm its validity.
 The check digit is validated using the Modulus 11 algorithm.

# NHSNumber - Record with a number attribute in order to constrain invariants (e.g. a 9 digit long List Int would be
 fundamentally invalid)

@docs NHSNumber

# Validators

@docs validateFromString, validateFromList

# Formatters

@docs show

-}

import Char exposing (isDigit)
import Array exposing (toList)


{-| NHSNumber is a record type with a number property, constraining the requirements of a 10 digit list of integers 0-9
 by hiding the constructor

-}
type alias NHSNumber = { number : (List Int) }


requiredNHSNumberLength = 10


charToInt : Char -> Int
charToInt c =
    c
    |> String.fromChar
    |> String.toInt
    |> Result.withDefault -1


stringToDigits : String -> List Int
stringToDigits s =
        s
        |> String.toList
        |> List.filter Char.isDigit
        |> List.map charToInt


{-| Converts an NHSNumber (List Int) to a 3-3-4 digit String.  Responsibility is on the consumer to ensure
a valid NHSNumber representation is passed in

-}
show : NHSNumber -> String
show { number } =
    let
        numbers = Array.fromList number
        sliceToString start end numbers =
            numbers
            |> Array.slice start end
            |> Array.toList
            |> List.map toString
            |> String.join ""
    in
        (sliceToString 0 3 numbers) ++ "-" ++ (sliceToString 3 6 numbers) ++ "-" ++ (sliceToString 6 10 numbers)


{-| Validate a string based potential NHS number.  Will ignore non-digit characters.

    validateFromString "1234567890"
    validateFromString "123-456-7890"

-}
validateFromString : String -> Result String NHSNumber
validateFromString raw =
    let
        check raw =
         raw
         |> stringToDigits
         |> isValidNHSNumber
    in
    case check raw of
        Err e -> Err e
        Ok number -> Ok (NHSNumber number)


{-| Validate a list of digits as a potential NHS number.

    validateFromList [1,2,3,4,5,6,7,8,9,0]

-}
validateFromList : List Int -> Result String NHSNumber
validateFromList numbers =
    let
        check n =
             n
             |> isValidNHSNumber
    in
    case check numbers of
        Err e -> Err e
        Ok number -> Ok (NHSNumber number)


calculateWeighting : List Int -> List Int
calculateWeighting number =
    let
        weightingFactors = [10,9,8,7,6,5,4,3,2]
    in
        List.map2 (*) weightingFactors number


checkDigitIsValid : List Int -> Result String (List Int)
checkDigitIsValid number =
    let
        getCheckDigit number =
            number
            |> List.drop 9
            |> List.head
            |> Maybe.withDefault -1
        convertEleven x =
            if x == 11 then 0 else x
        calculateCheckDigit number =
            number
            |> List.take 9
            |> calculateWeighting
            |> List.sum
            |> flip (%) 11
            |> (-) 11
            |> convertEleven
    in
        case (calculateCheckDigit number) == (getCheckDigit number) of
            True -> Ok number
            False -> Err "Invalid check digit"


isValidNHSNumberLength : List Int -> Result String (List Int)
isValidNHSNumberLength n =
    case requiredNHSNumberLength == List.length n of
        True -> Ok n
        False -> Err ("Invalid NHS Number length, should be " ++ (toString requiredNHSNumberLength))


isValidNHSNumber : List Int -> Result String (List Int)
isValidNHSNumber nhsNumber =
    let
        filterToValidNumberSet n = List.filter (\x -> x >= 0 && x < 10) n
    in
        nhsNumber
        |> filterToValidNumberSet
        |> isValidNHSNumberLength
        |> Result.andThen checkDigitIsValid
