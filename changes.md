# change log

## 1.0.0

Changes from [`lambda-phi/parser`](https://dark.elm.dmy.fr/packages/lambda-phi/parser/latest/)

  - `Parser.Expression` remove
  - `Parser.Sequence` merge → `Parser`
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
        merge into
        `split : TODO`
  - `Parser.Common` merge → `Text.Parser`
      - `digits` remove
          - in favor of `atLeast 0 Char.Parser.digit` explicitly
      - `letters` remove
          - in favor of `atLeast 0 Char.Parser.letter` explicitly
      - `spaces` remove
          - in favor of `atLeast 0 Char.Parser.blank` explicitly
      - `token` token
          - in favor of
                import Char.Parser as Char
                succeed (\... -> ...)
                    |> drop (atLeast 0 Char.blank)
                    |> take ...
                    |> drop (atLeast 0 Char.blank)
            explicitly
      - `textNoCase` name → `caseAny`
      - `line`
        `: Parser String`
        →
        `: Parser Char onLine -> Parser Char onLine`
  - `Parser.Check` remove
      - lookahead/-before remove
      - no `end` in favor of `narrowWith : ... -> Result (ExpectationMiss ... | InputRemaining ...)`
      - only `endOfLine` kept → consuming `lineEnd`
  - name `Parser.Char` → `Char.Parser`
      - `anyChar` remove
          - in favor of `Parser.atomAny`
      - `char` remove
          - in favor of `Parser.atom`
      - `charNoCase` name → `caseAny`
      - `lowercase` name → `caseLower`
      - `uppercase` name → `caseUpper`
      - `alphaNum` remove
          - in favor of `Parser.onFailDown [ letter, digit ]`
      - `space` name → `blank`
          - to emphasize it can be any whitespace
      - `except` remove
          - in favor of `Parser.except`
  - `Parser`
      - `parse : String -> Parser narrow -> Result Error narrow`
        →
        `narrowWith : Parser narrow -> String -> Result Error narrow`
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
