## 2.0.0 plan


# change log

## 1.0.0

changes from [`lambda-phi/parser`](https://dark.elm.dmy.fr/packages/lambda-phi/parser/latest/)

  - `Parser` → `MorphRow` that can also `build`
  - `Parser.Expression` remove
  - `Parser.Sequence` merge → `MorphRow`
      - `zeroOrMore` remove
          - in favor of `atLeast 0`
      - `oneOrMore` remove
          - in favor of `atLeast 1`
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
        to
        ```elm
        before :
            { end : MorphRow atom () expectedCustom
            , goOn : MorphRow atom goOnElement expectedCustom
            }
            -> MorphRow atom (List goOnElement) expectedCustom
        ```
        and
        ```elm
        until :
            { commit :
                Morph
                    commitResult
                    { end : endElement
                    , before : List goOnElement
                    }
                    (Morph.Error atom expectedCustom)
            , end : MorphRow atom endElement expectedCustom
            , goOn : MorphRow atom goOnElement expectedCustom
            }
            -> MorphRow atom commitResult expectedCustom
        ```
      - `split`, `splitIncluding`
        replace with
        ```elm
        separatedBy :
            ( MorphRow atom { separator : separator, part : part } expectedCustom
              -> MorphRow atom (List { separator : separator, part : part }) expectedCustom
            , MorphRow atom separator expectedCustom
            )
            -> MorphRow atom part expectedCustom
            ->
                MorphRow
                    atom
                    (Emptiable
                        (StackTopBelow
                            part
                            { separator : separator, part : part }
                        )
                        Never
                        Empty
                    )
                    expectedCustom
        ```
  - `Parser.Common` merge → `Morph.TextRow`
      - `digits` remove
          - in favor of `atLeast 0 (Digit.Morph.n0To9 |> one)` explicitly
      - `letters` remove
          - in favor of `atLeast 0 (Morph.Char.aToZ |> one)` explicitly
      - `spaces` remove
          - in favor of `atLeast 0 (Morph.Char.blank |> one)` explicitly
      - `token` token
          - in favor of
            ```elm
            import Morph.Char as Char
            succeed (\... -> ...)
                |> skip (atLeast 0 (Morph.Char.blank |> one))
                |> grab ...
                |> skip (atLeast 0 (Morph.Char.blank |> one))
            ```
            explicitly
      - `textNoCase` name → `caseAny`
      - `line` remove
        in favor of `... |> skip Morph.Text.lineEnd`
  - `Parser.Check` remove
      - `end` move → `MorphRow`
      - lookahead/-before remove
      - no `end` in favor of `narrowWith : ... -> Result (ExpectationMiss ... | InputRemaining ...)`
      - only `endOfLine` kept → consuming `lineEnd`
  - name `Parser.Char` → `Morph.Char`
      - `anyChar` remove
          - in favor of `MorphRow.oneAny`
      - `char` remove
          - in favor of `Morph.Text.specific`
      - `charNoCase` name → `caseAny`
      - `lowercase` name → `caseLower`
      - `uppercase` name → `caseUpper`
      - `alphaNum` remove
          - in favor of `Morph.choice ... |> MorphRow.possibility |> MorphRow.choiceFinish`
      - `space` name → `blank`
          - to emphasize it can be any whitespace
      - `except` remove
          - in favor of `Morph.choice ... |> MorphRow.possibility |> MorphRow.choiceFinish`
  - `MorphRow`
      - `parse : String -> MorphRow narrow -> Result Error narrow` remove
          - in favor of
            `Morph.narrow (... |> Morph.over Stack.Morph.toText)`
      - `first |> orElse second` remove
          - in favor of `onFailDown [ first, second ]`
      - can parse any input list (not only `String`)
      - `map2`, `map3`, `map4`, `map5` remove
          - in favor of `succeed |> grab |> grab ...`
      - `andThenKeep`, `andThenIgnore` remove
          - in favor of `grab`, `skip`
      - `andThen2` remove
      - `textOf` remove
          - in favor of `Morph.Text.fromList |> MorphRow.over ...`
      - `oneAny` add
      - `one` add
      - `before` add
      - `until` add
      - `while` add
      - `oneOf` remove
          - in favor of `Morph.choice ... |> MorphRow.possibility |> MorphRow.choiceFinish`
      - `possibility` add
      - `choiceFinish` add
