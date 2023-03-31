module Morph.Test exposing (tests)

import Email
import Expect
import Morph exposing (toBroad, toNarrow)
import Point
import Stack.Morph
import Test exposing (Test, test)
import Tree


tests : Test
tests =
    Test.describe
        "Morph to row"
        [ pointTest
        , emailTest
        ]



-- point


pointTest : Test
pointTest =
    Test.describe "point"
        [ test "fail"
            (\() ->
                Expect.fail """
What could have gone wrong in case of an overflow?

Maybe the lossy conversion
hasn't been set up properly (see DecimalOrException.float).

Another option is a potentially missed chance to do TCO.
This could have happened in many places, maybe because `|>` is used.
"""
            )
        , test "toNarrow |> toBroad"
            (\() ->
                let
                    narrowResult =
                        "(3.00,  -9999.1240)"
                            |> toNarrow
                                (Point.chars
                                    |> Morph.rowFinish
                                    |> Morph.over Stack.Morph.string
                                )
                in
                case narrowResult of
                    Err error ->
                        Morph.descriptionAndErrorToTree (Point.chars |> Morph.description)
                            (error |> Just)
                            |> Tree.map .text
                            |> Morph.treeToLines
                            |> String.join "\n"
                            |> Expect.fail

                    Ok narrow ->
                        narrow
                            |> toBroad
                                (Point.chars
                                    |> Morph.rowFinish
                                    |> Morph.over Stack.Morph.string
                                )
                            |> Expect.equal "( 3., -9999.124 )"
            )
        ]


emailTest : Test
emailTest =
    let
        emailToText =
            Email.chars |> Morph.rowFinish |> Morph.over Stack.Morph.string
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
