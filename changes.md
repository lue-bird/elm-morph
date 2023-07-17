## TODO

  - swap `MorphRowIndependently beforeToBroad narrow broadElement` to `MorphRowIndependently narrow beforeToBroad broadElement`
  - add more tests

# change log

## 1.0.0

changes from [`lambda-phi/parser`](https://dark.elm.dmy.fr/packages/lambda-phi/parser/latest/)
(definitely incomplete)

  - `Parser` → `MorphRow` that can also build
  - `Parser.Expression` remove
  - `Parser.Sequence` merge into `MorphRow`
      - `zeroOrMore` remove
          - in favor of `atLeast n0`/`whilePossible`
      - `oneOrMore` remove
          - in favor of `atLeast n1`
      - `zeroOrOne` remove
          - in favor of `choice` or `Maybe.Morph.row` or `in_ ( n0, n1 )`
      - `atMost max` remove
          - in favor of `in_ ( n0, max )`
      - `concat` remove
      - `fold`, `foldWhile` remove
          - in favor of `whileAccumulate`
      - `atLeast`, `in_`, `exactly` return `typesafe-array`
      - replaced
        ```elm
        until :
            Parser delimiter
            -> Parser a
            -> Parser ( List a, delimiter )
        ```
        → `before` to ignore the end and `until` to only commit conditionally
      - `split`, `splitIncluding` remove
          - in favor of
            ```elm
            Morph.succeed (\first separatedElements -> { first = first, separatedElements = separatedElements })
                |> grab .first element
                |> grab .separatedElements
                    (Morph.whilePossible
                        (Morph.succeed (\separator element -> { element = element, separator = separator })
                            |> grab .separator separator
                            |> grab .element element
                        )
                    )
            ```
  - `Parser.Common` merge → `String.Morph`
      - `digits` remove
          - in favor of `atLeast n0 (Digit.n0To9 |> one)` explicitly
      - `letters` remove
          - in favor of `atLeast n0 (Morph.AToZ.caseAny |> one)` explicitly
      - `spaces` remove
          - in favor of `atLeast n0 (blankChar |> one)` explicitly
            where `blankChar` is a custom definition or just `Char.Morph.only ' '`
      - `token` token
          - in favor of
            ```elm
            import Char.Morph as Char
            Morph.succeed (\... -> ...)
                |> match (atLeast n0 (String.Morph.only " "))
                |> grab ...
                |> match (atLeast n0 (String.Morph.only " "))
            ```
            explicitly
      - `textNoCase` name → `caseAny`
      - `line` remove
        in favor of `... |> match String.Morph.lineEnd`
  - `Parser.Check` remove
      - `end` move → `MorphRow`
      - lookahead/-before remove
      - no `end` in favor of `toNarrow : ... -> Result (ExpectationMiss ... | InputRemaining ...)`
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
          - in favor of `Choice.between ... |> Choice.rowTry |> Morph.choiceFinish`
      - `space` name → `blank`
          - to emphasize it can be any whitespace
      - `except` remove
          - in favor of `Choice.between ... |> Choice.rowTry |> Morph.choiceFinish`
  - `MorphRow`
      - `parse : String -> MorphRow narrow -> Result Error narrow` remove
          - in favor of
            `toNarrow (... |> Morph.over Stack.Morph.toText)`
      - `first |> orElse second` remove
          - in favor of `onFailDown [ first, second ]`
      - can parse any input list (not only `String`)
      - `map2`, `map3`, `map4`, `map5` remove
          - in favor of `Morph.succeed |> grab |> grab ...`
      - `andThenKeep`, `andThenIgnore` remove
          - in favor of `grab`, `match`
      - `andThen2` remove
      - `textOf` remove
          - in favor of `String.Morph.list |> Morph.overRow ...`
      - `oneAny` add
      - `one` add
      - `before` add
      - `until` add
      - `while` add
      - `oneOf` remove
          - in favor of `Choice.between ... |> Choice.rowTry |> Morph.choiceFinish`
      - `possibility` add
      - `choiceFinish` add
