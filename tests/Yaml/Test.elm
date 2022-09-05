module Yaml.Test exposing (tests)

{-| TODO <https://github.com/yaml/yaml-test-suite/>
-}

import Expect
import Test exposing (Test, test)


tests : Test
tests =
    Test.describe "YAML"
        [ test "example"
            (\() ->
                """"""
                    |> Expect.equal ""
            )
        ]
