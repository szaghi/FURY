---
title: Features
---

# Features

## The three steps

Every FURY computation follows the same steps: define the units, attach them to numbers, compute.

<<< @/examples/snippets/bolt.f90

<<< @/examples/output/bolt.ansi{ansi}

## Feature map

| Area | Features | Where |
|---|---|---|
| **Units** | units from a string: symbols, synonyms, conversion aliases, dimensions, compound units of reference units, main alias, name; printing with or without each part | [Unit grammar](./grammar), [Units](./units) |
| **Algebra** | products, quotients, integer and real powers of units, with cancelling exponents and derived dimensions; comparisons `==`, `/=` | [Units](./units#algebra) |
| **Quantities** | a real magnitude, its unit and a name; constructor, `number * unit`; arithmetic with quantities and numbers of any kind; powers; comparisons; formatted printing | [Quantities](./quantities) |
| **Consistency** | sums, differences, assignments and conversions of inconsistent units stop the program (`error stop`, exit status 1); dimensions checked against the exponents of the symbols | [Errors](./errors) |
| **Conversions** | factors, protected factors, offsets, several aliases per unit; conversions through a common alias and through the main alias of a compound unit; user-supplied non-linear converters | [Conversions](./conversions) |
| **Systems** | the SI system: 42 units, 28 decimal and binary prefixes, 15 constants (2019 SI, CODATA 2018); queries by name, symbol, synonym, main alias or prefixed symbol; systems of your own | [Units systems](./systems) |
| **Precision** | every type in 32, 64 and 128 bits; mixed-kind arithmetic, comparisons and assignments; optional quadruple precision | [Precision](./precision) |
| **Builds** | FoBiS (static and shared libraries, gfortran and Intel), fpm; tests; a converter app | [Installation](./install) |

## The types

| Type | Module | Purpose |
|---|---|---|
| `uom32`, `uom64`, `uom128` | `fury` | unit of measure |
| `qreal32`, `qreal64`, `qreal128` | `fury` | physical quantity |
| `system_si32`, `system_si64`, `system_si128` | `fury` | the SI system |
| `system_abstract32`, `system_abstract64`, `system_abstract128` | `fury` | the base of a units system |
| `uom_converter` | `fury` | the base of a user-supplied converter |
| `uom_reference*`, `uom_symbol*` | `fury` | the building blocks of a unit: a reference unit and a symbol |

`fury` also exports the kind parameters of [PENF](https://github.com/szaghi/PENF): `R4P`, `R8P`, `R16P`, `R_P`, `I1P`,
`I2P`, `I4P`, `I8P`, `I_P`, and its `str`, `strz` functions.
