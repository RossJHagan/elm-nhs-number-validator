module Tests exposing (..)

import Test exposing (..)
import Expect
import String

import NHSNumber

validNHSNumbers =
    [ "495-579-0062"
    , "733 095 7763"
    , "739-516-3621"
    , "688-609-4858"
    , "552-043-7793"
    , "1554193974"
    , "187-821-7429"
    , "713-350-0213"
    , "478-574-7102"
    , "626-921-2200"
    ]

invalidNHSNumbers =
    [ "495-579-0060"
    , "733-095-7760"
    , "739-516-3620"
    , "688-609-4850"
    , "552-043-7790"
    , "155-419-3970"
    , "187-821-7420"
    , "713-350-0210"
    , "478-574-7100"
    , "626-921-2201"
    , ""
    , "123"
    ]


passNumber n =
    case NHSNumber.validateFromString n of
        Ok _ -> Expect.pass
        Err e -> Expect.fail e


nhsNumberSuite : Test
nhsNumberSuite =
    describe "NHSNumber module"
        [ describe "NHSNumber.validateNHSNumberString"
            [ test "correctly validates string NHS Numbers" <|
                \() -> Expect.true "Expect numbers to be valid" (List.all (\x -> x == Expect.pass) (List.map passNumber validNHSNumbers))
            , test "fails to validate invalid string NHS Numbers" <|
                \() -> Expect.true "Expect numbers to all be invalid" (List.all (\x -> x /= Expect.pass) (List.map passNumber invalidNHSNumbers))
            ]
        , describe "NHSNumber.fromList"
            [ test "correctly parses a valid NHS Number from a list of integers" <|
                \() -> (case NHSNumber.validateFromList [7,3,9,5,1,6,3,6,2,1] of
                                    Ok _ -> Expect.pass
                                    Err e -> Expect.fail e)
            , test "fails to parse an invalid (bad check digit) list of integers to NHS Number" <|
                \() -> (case NHSNumber.validateFromList [7,3,9,5,1,6,3,6,2,0] of
                                    Ok _ -> Expect.fail "Should not return OK from NHSNumber.fromList with a bad sequence"
                                    Err _ -> Expect.pass)
            , test "fails to parse an invalid (out of range) list of integers to NHS Number" <|
                \() -> (case NHSNumber.validateFromList [17,3,9,5,1,6,3,6,2,0] of
                                    Ok _ -> Expect.fail "Should not return OK from NHSNumber.fromList with a bad sequence"
                                    Err _ -> Expect.pass)
            ]
        , describe "NHSNumber.show"
            [ test "correctly displays an NHS Number" <|
                \() -> Expect.equal "123-456-7890" (NHSNumber.show [1,2,3,4,5,6,7,8,9,0])
            ]
        ]


