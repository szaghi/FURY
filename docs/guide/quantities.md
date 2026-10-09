---
title: Quantities
---

# Quantities

`qreal64` (and `qreal32`, `qreal128`) is a physical quantity: a real magnitude, its unit and an optional name. The
components are public:

| Component | Type |
|---|---|
| `magnitude` | `real(R8P)` (`R4P`, `R16P`) |
| `unit` | `class(uom64), allocatable` |
| `name` | `character(len=:), allocatable` |

## Creating a quantity

<<< @/examples/snippets/cookbook_attach-attach.f90

`qreal64(magnitude, unit, name)`, all optional; `number * unit` for any integer or real kind; `system%qunit(name)` a
quantity of magnitude 1 in a unit of a [system](./systems).

## Operators

| Operation | Operands | Result |
|---|---|---|
| `q + r`, `q - r` | quantities with equal units | the sum, in that unit; different units are an [error](./errors) |
| `+q`, `-q` | a quantity | the quantity, its opposite |
| `q * r`, `q / r` | quantities | the product, the quotient, with the [unit algebra](./units#algebra) |
| `q * x`, `x * q`, `q / x` | a quantity and a number of any integer or real kind | the scaled quantity |
| `q ** n`, `q ** x` | a quantity and an integer or a real | the power; for `1/q` write `q ** (-1)` |
| `q == r`, `q /= r` | quantities | equality of magnitudes and units |
| `q = r` | quantities | assignment: a quantity without unit takes the one of `r`; one with a unit accepts only an equal unit, otherwise it is an [error](./errors) |

Quantities of different kinds mix: see [Precision](./precision).

## Methods

| Method | Purpose |
|---|---|
| `stringify(format, with_dimensions, with_aliases, with_name, compact_reals)` | the quantity as a string: `format` is an edit descriptor for the magnitude, the flags choose the parts of the unit, `with_name` prefixes the name |
| `to(unit)` | the quantity converted into `unit`; an impossible conversion is an [error](./errors) |
| `has_same_unit(other)` | true if the units are equal |
| `is_unit_defined()` | true if the quantity has a unit |
| `has_name()` | true if the quantity has a name |
| `set(magnitude, unit, name)` | sets the parts |
| `unset()` | frees the quantity: it can be assigned any unit again |
| `allocate_unit()` | allocates the unit component |

<<< @/examples/snippets/cookbook_print-print.f90

<<< @/examples/output/cookbook_print.ansi{ansi}
