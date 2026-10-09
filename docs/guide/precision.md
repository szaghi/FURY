---
title: Precision
---

# Precision

## Kinds

| Bits | Real kind | Types |
|---|---|---|
| 32 | `R4P` | `uom32`, `qreal32`, `system_si32`, `system_abstract32`, `uom_reference32`, `uom_symbol32` |
| 64 | `R8P` | `uom64`, `qreal64`, `system_si64`, `system_abstract64`, `uom_reference64`, `uom_symbol64` |
| 128 | `R16P` | `uom128`, `qreal128`, `system_si128`, `system_abstract128`, `uom_reference128`, `uom_symbol128` |

The kinds are the ones of [PENF](https://github.com/szaghi/PENF): `R4P = selected_real_kind(6, 37)`,
`R8P = selected_real_kind(15, 307)`, `R16P = selected_real_kind(33, 4931)`.

## Mixed kinds

Quantities of different kinds can be summed, subtracted, multiplied, divided and compared: the result has the kind of
the more precise operand, as for the intrinsic reals. A quantity of a kind is assigned to one of another kind, converting
its magnitude; so are units, references and symbols.

<<< @/examples/snippets/tutorial_7-mixed.f90

## Quadruple precision

The 128 bits types exist only when FURY and PENF are compiled with the macro `PENF_R16P`:

| Build | Quadruple precision |
|---|---|
| FoBiS, every mode but `*-noquad` | yes, `-DPENF_R16P` |
| FoBiS, `fury-static-gnu-noquad`, `tests-gnu-noquad` | no |
| fpm | no: fpm cannot pass a macro to a dependency |

Without `PENF_R16P`, PENF defines `R16P` as a double precision kind, and FURY compiles out its 128 bits types: they are
not defined, rather than being double precision in disguise, and a program that uses them does not compile. FURY tests
PENF's own macro, so the two always agree.

A user-supplied [converter](./conversions#user-supplied-converters) implements `convert_float128` in both cases, so that
the same code compiles with and without quadruple precision.
