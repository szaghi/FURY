---
title: Units
---

# Units

`uom64` (and `uom32`, `uom128`) is a unit of measure: a product of reference units, an optional main alias and an
optional name.

## Creating a unit

```fortran
type(uom64) :: newton
newton = uom64('kg [mass].m [length].s-2 [time-2] (N[force]) {newton}')
```

`uom64(source, alias, name)`: `source` is a definition in the [unit grammar](./grammar); `alias` (a `uom_reference64`)
and `name` override the ones of the source.

## Algebra

| Operation | Result |
|---|---|
| `u * v`, `u / v` | the product or quotient of the references: equal symbols add their exponents, symbols with exponent 0 are removed (unless all are), dimensions follow; the main aliases are dropped, the names combined |
| `u ** n` | the power, integer or real; a real power must give integer exponents, otherwise it is an [error](./errors) |
| `u + v`, `u - v` | the unit itself, if `u == v`, otherwise an [error](./errors): the unit of a sum |
| `u == v`, `u /= v` | equality of the references (symbols and exponents) |
| `u = v` | assignment: an undefined unit takes `v`; a defined one accepts only an equal unit, otherwise it is an [error](./errors) |

<<< @/examples/snippets/tutorial_3-algebra.f90

## Methods

| Method | Purpose |
|---|---|
| `stringify(with_dimensions, with_aliases, protect_aliases, with_name, compact_reals)` | the unit as a string, with the parts asked |
| `is_defined()` | true if the unit has at least one reference |
| `has_name()`, `has_alias()` | true if the unit has a name, a main alias |
| `get_main_symbol()` | the main symbol: the one of the main alias, or of the only reference |
| `get_references(references)`, `get_alias(alias)`, `get_main_reference()` | the building blocks |
| `has_reference(reference)` | true if the unit has the reference |
| `convert(other, magnitude, converted, is_converted)` | converts a magnitude into `other`, reporting if the conversion exists (see [Conversions](./conversions)) |
| `to(other, magnitude)` | the converted magnitude, unchanged if the conversion does not exist |
| `prefixed(prefixes)` | the unit prefixed by a prefix (a `uom_reference`), as the units systems do |
| `set(references, alias, name)` | sets the parts |
| `set_alias_conversion(reference_index, alias_index, convert)` | attaches a [converter](./conversions#user-supplied-converters) to an alias |
| `unset()` | frees the unit: it can be assigned any unit again |

The component `name` is public.
