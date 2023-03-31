## why have separate `Decimal` and `Decimal.Morph` modules for example?

This way, no import cycles are created (for example with [`Integer.Morph.decimal`](Integer#decimal) and [`Decimal.truncate`](Decimal#truncate)) while safe internals are still exposed.

It also creates nice consistency between for example `Stack` and `Stack.Morph`,
`Stack` being in a separate, independent package and `Stack.Morph` being what `elm-morph` builds on top.
Maybe the types like `Decimal` and helpers will one day be in their own library, too and only the `Morph`s will stay here.
