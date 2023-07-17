module Morph.Test exposing (tests)

import Email
import Expect
import Fuzz exposing (Fuzzer)
import List.Morph
import Morph exposing (toBroad, toNarrow)
import N exposing (n0, n1, n9)
import NaturalAtLeast1Base10 exposing (NaturalAtLeast1Base10)
import Point
import Test exposing (Test, test)
import Tree


tests : Test
tests =
    Test.describe
        "MorphRow"
        [ pointTest
        , emailTest
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
