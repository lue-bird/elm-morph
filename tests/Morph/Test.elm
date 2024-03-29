module Morph.Test exposing (tests)

import Boolean exposing (Boolean(..))
import Email
import Expect
import Fuzz exposing (Fuzzer)
import Integer
import Json.Morph
import List.Morph
import Morph exposing (toBroad, toNarrow)
import N exposing (n0, n1, n9)
import Natural
import NaturalAtLeast1Base10 exposing (NaturalAtLeast1Base10)
import Point
import Project
import Test exposing (Test, test)
import Tree
import Value.Morph


tests : Test
tests =
    Test.describe
        "MorphRow"
        [ pointTest
        , emailTest
        , booleanTest
        , projectTest
        , Test.fuzz naturalAtLeast1Base10Fuzz
            "base10: = to |> from Base2"
            (\naturalAtLeast1Base10 ->
                ( naturalAtLeast1Base10
                , -- only added for debugging
                  naturalAtLeast1Base10 |> NaturalAtLeast1Base10.toBase2
                )
                    |> Expect.equal
                        ( naturalAtLeast1Base10
                            |> NaturalAtLeast1Base10.toBase2
                            |> NaturalAtLeast1Base10.fromBase2
                        , naturalAtLeast1Base10 |> NaturalAtLeast1Base10.toBase2
                        )
            )
        , Test.fuzz Fuzz.int
            "Integer: = to |> from Int"
            (\naturalAtLeast1Base10 ->
                naturalAtLeast1Base10
                    |> Expect.equal
                        (naturalAtLeast1Base10
                            |> Integer.fromInt
                            |> Integer.toInt
                        )
            )
        , Test.fuzz (Fuzz.map N.intToAbsolute Fuzz.int)
            "Natural: = to |> from N"
            (\naturalAtLeast1Base10 ->
                ( naturalAtLeast1Base10 |> N.toInt
                , -- only added for debugging
                  naturalAtLeast1Base10 |> Natural.fromN
                )
                    |> Expect.equal
                        ( naturalAtLeast1Base10
                            |> Natural.fromN
                            |> Natural.toN
                            |> N.toInt
                        , naturalAtLeast1Base10 |> Natural.fromN
                        )
            )
        ]


naturalAtLeast1Base10Fuzz : Fuzzer NaturalAtLeast1Base10
naturalAtLeast1Base10Fuzz =
    Fuzz.constant (\first afterFirst -> { first = first, afterFirst = afterFirst })
        |> Fuzz.andMap (Fuzz.map N.inToNumber (N.inFuzzUniform ( n1, n9 )))
        |> Fuzz.andMap
            (Fuzz.listOfLengthBetween 0
                9
                (Fuzz.map N.inToNumber (N.inFuzzUniform ( n0, n9 )))
            )



-- point


pointTest : Test
pointTest =
    Test.describe "point"
        [ test "toNarrow |> toBroad"
            (\() ->
                let
                    narrowResult =
                        "(3.00,  -9999.1240)"
                            |> toNarrow
                                (Point.chars
                                    |> Morph.rowFinish
                                    |> Morph.over List.Morph.string
                                )
                in
                case narrowResult of
                    Err error ->
                        Morph.descriptionAndErrorToTree (Point.chars |> Morph.description) error
                            |> Tree.map .text
                            |> Morph.treeToLines
                            |> String.join "\n"
                            |> Expect.fail

                    Ok narrow ->
                        narrow
                            |> toBroad
                                (Point.chars
                                    |> Morph.rowFinish
                                    |> Morph.over List.Morph.string
                                )
                            |> Expect.equal "( 3., -9999.124 )"
            )
        ]


emailTest : Test
emailTest =
    let
        emailToText =
            Email.chars |> Morph.rowFinish |> Morph.over List.Morph.string
    in
    Test.describe
        "email"
        [ Test.describe
            "valid"
            ([ """simple@example.com"""
             , """very.common@example.com"""
             , """other.email-with-hyphen@example.com"""
             , """fully-qualified-domain@example.com"""
             , -- one-letter local-part
               """x@example.com"""
             , """example-indeed@strange-example.com"""
             , -- slashes are an allowed printable character
               """test/test@test.com"""
             , -- local domain name with no TLD, although ICANN highly discourages dotless email addresses
               """admin@mailserver1"""
             , """example@s.example"""
             , """user-@example.org"""
             ]
                |> List.map
                    (\exampleEmail ->
                        test
                            exampleEmail
                            (\() ->
                                case exampleEmail |> toNarrow emailToText of
                                    Ok emailParsed ->
                                        emailParsed
                                            |> toBroad emailToText
                                            |> Expect.equal exampleEmail

                                    Err _ ->
                                        Expect.pass
                            )
                    )
            )
        , Test.describe
            "invalid"
            ([ -- no @ character
               """Abc.example.com"""
             , -- only one @ is allowed
               """A@b@c@example.com"""
             , -- Underscore is not allowed in domain part
               """i_like_underscore@but_its_not_allowed_in_this_part.example.com"""
             , """QA[icon]CHOCOLATE[icon]@test.com"""
             ]
                |> List.map
                    (\exampleEmail ->
                        test
                            exampleEmail
                            (\() ->
                                case exampleEmail |> toNarrow emailToText of
                                    Ok _ ->
                                        Expect.fail exampleEmail

                                    Err _ ->
                                        Expect.pass
                            )
                    )
            )
        ]


booleanTest : Test
booleanTest =
    test "Boolean.char succeeds correctly"
        (\() ->
            "((true || false) || false)"
                |> Morph.toNarrow
                    (Boolean.chars
                        |> Morph.rowFinish
                        |> Morph.over List.Morph.string
                    )
                |> Expect.equal
                    (Ok (BooleanOr { left = BooleanOr { left = BooleanTrue, right = BooleanFalse }, right = BooleanFalse }))
        )


projectTest : Test
projectTest =
    test "prints correctly as compact json string"
        (\() ->
            { name = "example", description = "increment and decrement" }
                |> Morph.toBroad
                    (Project.morphValue
                        |> Morph.over (Value.Morph.eachTag Json.Morph.compact)
                        |> Morph.over Value.Morph.json
                        |> Morph.over Json.Morph.string
                    )
                |> Expect.equal
                    """{"a1":"increment and decrement","a0":"example"}"""
        )
