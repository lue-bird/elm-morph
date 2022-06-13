## 2.0.0 plan

  - `atLeast`, `between`, `exactly`, `split` return `typesafe-array`

# change log

## 1.0.0

Changes from [`lambda-phi/parser`](https://dark.elm.dmy.fr/packages/lambda-phi/parser/latest/)

  - `Parser` → `ConversionStep` that can also `build`
  - `ConversionStep.Expression` remove
  - `ConversionStep.Sequence` merge → `ConversionStep`
      - `zeroOrMore` remove
          - in favor of `atLeast 0`
      - `oneOrMore` remove
          - in favor of `atLeast 1`
      - `zeroOrOne` remove
          - in favor of `between 0 1`
      - `atMost max` remove
          - in favor of `between 0 max`
      - `concat` remove
          - in favor of `map List.concat`
      - `fold`, `foldWhile` remove
          - in favor of `loop`
      - `split`, `splitIncluding`
        replace with
        ```elm
        split :
            ConversionStep atom separator
            -> ConversionStep atom part
            ->
                ConversionStep
                    atom
                    { initial : List { part : List atom, separator : separator }
                    , last : List atom
                    }
        ```
  - `ConversionStep.Common` merge → `Text.ConversionStep`
      - `digits` remove
          - in favor of `atLeast 0 Char.ConversionStep.digit` explicitly
      - `letters` remove
          - in favor of `atLeast 0 Char.ConversionStep.letter` explicitly
      - `spaces` remove
          - in favor of `atLeast 0 Char.ConversionStep.blank` explicitly
      - `token` token
          - in favor of
                import Char.ConversionStep as Char
                succeed (\... -> ...)
                    |> drop (atLeast 0 Char.blank)
                    |> take ...
                    |> drop (atLeast 0 Char.blank)
            explicitly
      - `textNoCase` name → `caseAny`
      - `line`
        `: ConversionStep String`
        →
        `: ConversionStep Char onLine -> ConversionStep Char onLine`
  - `ConversionStep.Check` remove
      - lookahead/-before remove
      - no `end` in favor of `narrowWith : ... -> Result (ExpectationMiss ... | InputRemaining ...)`
      - only `endOfLine` kept → consuming `lineEnd`
  - name `ConversionStep.Char` → `Char.ConversionStep`
      - `anyChar` remove
          - in favor of `ConversionStep.atomAny`
      - `char` remove
          - in favor of `ConversionStep.atom`
      - `charNoCase` name → `caseAny`
      - `lowercase` name → `caseLower`
      - `uppercase` name → `caseUpper`
      - `alphaNum` remove
          - in favor of `ConversionStep.onFailDown [ letter, digit ]`
      - `space` name → `blank`
          - to emphasize it can be any whitespace
      - `except` remove
          - in favor of `ConversionStep.except`
  - `ConversionStep`
      - `parse : String -> ConversionStep narrow -> Result Error narrow`
        →
        `narrowWith : ConversionStep narrow -> String -> Result Error narrow`
      - `first |> orElse second` remove
          - in favor of `onFailDown [ first, second ]`
      - can parse any input list (not only `String`)
      - `map2`, `map3`, `map4`, `map5` remove
          - in favor of `succeed |> take |> take ...`
      - `andThenKeep/-Ignore` remove
          - in favor of `take`/`drop`
      - `andThen2` remove
          - in favor of `andThen ... (succeed |> take |> take)`
      - `textOf` remove
          - in favor of `map String.fromList`
      - `atomAny` add
      - `atom` add
      - loop add
      - `except` add
      - `oneOf` name → `onFailDown`
