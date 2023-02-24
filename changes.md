# change log

## 1.0.0

changes from [`lambda-phi/parser`](https://dark.elm.dmy.fr/packages/lambda-phi/parser/latest/)
(definitely incomplete)

  - `Parser` → `MorphRow` that can also build
  - `Parser.Expression` remove
  - `Parser.Sequence` merge → `MorphRow`
      - `zeroOrMore` remove
          - in favor of `atLeast ... n0`
      - `oneOrMore` remove
          - in favor of `atLeast n1`
      - `zeroOrOne` remove
          - in favor of `maybe`
      - `atMost max` remove
          - in favor of `between 0 max`
      - `concat` remove
      - `fold`, `foldWhile` remove
          - in favor of `whileAccumulate`
      - `atLeast`, `between`, `exactly`, `split` return `typesafe-array`
      - `until` changed
        ```elm
        until :
            Parser delimiter
            -> Parser a
            -> Parser ( List a, delimiter )
        ```
        →
        ```elm
        until :
            { commit :
                Morph
                    commitResult
                    { end : endElement
                    , before : List goOnElement
                    }
                    (Morph.Error broadElement expectedCustom)
            , end : MorphRow broadElement endElement expectedCustom
            , goOn : MorphRow broadElement goOnElement expectedCustom
            }
            -> MorphRow broadElement commitResult expectedCustom
        ```
        and its simpler cousin
        ```elm
        before :
            { end : MorphRow broadElement () expectedCustom
            , goOn : MorphRow broadElement goOnElement expectedCustom
            }
            -> MorphRow broadElement (List goOnElement) expectedCustom
        ```
      - `split`, `splitIncluding` remove
          - in favor of
            ```elm
            Morph.succeed
                |> grab element
                |> grab
                    (atLeast
                        (Morph.succeed
                            |> grab separator
                            |> grab element
                        )
                        n0
                    )
            ```
  - `Parser.Common` merge → `String.Morph`
      - `digits` remove
          - in favor of `atLeast (Digit.n0To9 |> one) n0` explicitly
      - `letters` remove
          - in favor of `atLeast (Morph.AToZ.caseAny |> one) n0` explicitly
      - `spaces` remove
          - in favor of `atLeast (blankChar |> one) n0` explicitly
            where `blankChar` is a custom definition or just `Char.Morph.only ' '`
      - `token` token
          - in favor of
            ```elm
            import Char.Morph as Char
            Morph.succeed (\... -> ...)
                |> skip (atLeast (String.Morph.only " ") n0)
                |> grab ...
                |> skip (atLeast (String.Morph.only " ") n0)
            ```
            explicitly
      - `textNoCase` name → `caseAny`
      - `line` remove
        in favor of `... |> skip String.Morph.lineEnd`
  - `Parser.Check` remove
      - `end` move → `MorphRow`
      - lookahead/-before remove
      - no `end` in favor of `narrowTo : ... -> Result (ExpectationMiss ... | InputRemaining ...)`
      - only `endOfLine` kept → consuming `lineEnd`
  - name `Parser.Char` → `Char.Morph`
      - `anyChar` remove
          - in favor of `Morph.oneAny`
      - `char` remove
          - in favor of `String.Morph.only`
      - `charNoCase` name → `caseAny`
      - `lowercase` name → `caseLower`
      - `uppercase` name → `caseUpper`
      - `alphaNum` remove
          - in favor of `Choice.between ... |> Choice.tryRow |> Morph.choiceRowFinish`
      - `space` name → `blank`
          - to emphasize it can be any whitespace
      - `except` remove
          - in favor of `Choice.between ... |> Choice.tryRow |> Morph.choiceRowFinish`
  - `MorphRow`
      - `parse : String -> MorphRow narrow -> Result Error narrow` remove
          - in favor of
            `narrowTo (... |> Morph.over Stack.Morph.toText)`
      - `first |> orElse second` remove
          - in favor of `onFailDown [ first, second ]`
      - can parse any input list (not only `String`)
      - `map2`, `map3`, `map4`, `map5` remove
          - in favor of `Morph.succeed |> grab |> grab ...`
      - `andThenKeep`, `andThenIgnore` remove
          - in favor of `grab`, `skip`
      - `andThen2` remove
      - `textOf` remove
          - in favor of `String.Morph.list |> Morph.rowOver ...`
      - `oneAny` add
      - `one` add
      - `before` add
      - `until` add
      - `while` add
      - `oneOf` remove
          - in favor of `Choice.between ... |> Choice.tryRow |> Morph.choiceRowFinish`
      - `possibility` add
      - `choiceFinish` add
