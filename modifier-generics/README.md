# modifier-generics

Modifier-aware `Generic` and polykinded `Generic1`, following the approach of
`linear-generics`: reuse stock generic structure while enriching its metadata.

Derive `GHC.Generics.Generic` / `Generic1` and enable
`-fplugin=Generics.Modifier.Plugin` in each defining module. Import
`Generics.Modifier` for `Generic`, `Generic1`, `Rep`, `Rep1`, conversions and
representation constructors. `MetaData`, `MetaCons` and `MetaSel` each add a
`[Modifier]` slot. `Mod` packages modifiers of arbitrary kinds without filtering,
reordering or deduplication.

`Rep1` metadata uses the polykinded symbolic `Parameter` for the datatype's final
argument. Other parameters are instantiated normally. The plugin also captures
annotations usable by `th-reify-modifier`.

See the [repository README](https://github.com/konn/modifier-macros#readme) for
examples, compiler requirements and boundaries. Tests cover sums, products,
newtypes, positional and grouped record fields, GADTs, parameterized modifiers,
and optimized generic consumers. Property tests use `falsify`; Core assertions
use `tasty-inspection-testing`.
