# change log

### 1.1.0

  - `Value.atomMap` add

#### 1.1.1

  - `Value.atomMap` documentation correct

## 2.0.0

  - `Morph.recursive` now supplies an argument that doesn't need to be applied with `()`
      - saw how to implement it [in elm-codec](https://github.com/miniBill/elm-codec/blob/2.0.0/src/Codec.elm#L866)

### 2.1.0

  - `module Tree.Morph exposing (value)` add
  - `Bit.Morph.value` add

## 3.0.0

  - type `Json tag` → `Json` with string tags just like the actual format
      - `Json.tagMap` remove
      - `Json.Morph.eachTag` remove
  - `Value.Morph.bits` now always print just the index
  - Rework tag morphs to convert to broad `String`
      - `Json.Morph.compact` now only prints valid json field names
      - `Value.Index`, `Value.Name` remove
  - `Integer.Morph`
      - `fromNatural` add
  - `Json`
      - `fromValue` add
      - `toValue` add
