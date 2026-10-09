---
title: Unit grammar
---

# Unit grammar

A unit is defined by a string. Its full form is

```
reference . reference . ... (main_alias [dimensions]) {name}
```

where each `reference` is

```
symbol = alias = alias ... [dimensions]
```

Only the first symbol is required: `'m'` is a valid unit. The parts are separated by spaces or by nothing.

## Symbols

A symbol is a name followed by an optional integer exponent: `m`, `m2`, `s-1`, `kg-1`. A negative exponent has its
sign, a positive one has none, an exponent 1 is omitted. The symbol and its exponent are the main symbol of a reference.

## Aliases

The symbols after `=` are the **aliases** of a reference. An alias is

| Form | Example | Meaning |
|---|---|---|
| `symbol` | `m = metre = meter` | a synonym: the same unit, another symbol |
| `factor * symbol` | `km = 1000 * m` | one `km` is `1000 m` |
| `offset + symbol` | `degC = 273.15 + K` | a temperature of `x degC` is `273.15 + x K` |
| `offset + factor * symbol` | `degF = 255.37222222222223 + 0.5555555555555556 * K` | linear, with offset |
| `@user symbol` | `dBm = @user mW` | a formula supplied by a [converter](./conversions#user-supplied-converters) |

The aliases are the conversion formulas of the reference: see [Conversions](./conversions). A factor cannot be zero.

### Protected aliases

The `.` separates references, so an alias with a decimal point must be **protected** between `<` and `>`:

```
in< = 0.0254 * m = inch> [length] {inch}
celsius< = 273.15 + K> [temperature] {celsius}
```

`stringify(with_aliases=.true., protect_aliases=.true.)` prints the aliases protected.

## Dimensions

`[dimensions]` after a reference gives the dimensions of its main symbol, with the same exponent: `m [length]`,
`s-2 [time-2]`, `m-1 [length-1]`. A different exponent is an [error](./errors): `Hz = s-1 [time-2]` does not parse.
The dimensions of a compound unit are derived from the ones of its references: `kg [mass].m [length].s-2 [time-2]` has
dimensions `mass.length.time-2`.

The dimensions are optional; a unit without dimensions is printed without them.

## Compound units

References separated by `.` multiply, each with its exponent: `kg.m.s-2` is $kg \cdot m \cdot s^{-2}$, as in
[The Unified Code for Units of Measure](https://ucum.org/).

## Main alias

`(symbol [dimensions])` after the references is the **main alias** of the unit: the short symbol a compound unit is known
by, with its own dimensions and aliases.

```
kg [mass].m [length].s-2 [time-2] (N[force]) {newton}
bit [bit].s-1 [time-1] (baud = Bd = bps) {baud}
```

The main alias is printed by `stringify(with_aliases=.true.)`, queried by the [units systems](./systems) and used by the
[conversions](./conversions).

## Name

`{name}`, always the last part, is the name of the unit: `{newton}`. Units systems query units by name.

## Examples

| Definition | |
|---|---|
| `m` | a symbol |
| `m [length]` | a symbol with dimensions |
| `m = metre = meter [length] {metre}` | synonyms and a name |
| `km = 1000 * m [length]` | a conversion factor |
| `mi< = 1609.344 * m = mile> [length] {mile}` | a protected factor and a synonym |
| `hour = 3600 * s = 60 * minute = hr [time]` | several conversions and a synonym |
| `celsius< = 273.15 + K> [temperature]` | an offset |
| `kg [mass].m-1 [length-1].s-2 [time-2] (Pa[pressure]) {pascal}` | a compound unit with main alias and name |
| `dBm = @user mW` | a user-supplied conversion |
