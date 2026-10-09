---
title: Conversions
---

# Conversions

`q%to(unit)` converts a quantity into another unit, through the conversion formulas of the [aliases](./grammar#aliases).

## How a conversion is found

A unit is converted **reference by reference**: every reference of the unit must be converted into a different
reference of the target unit (so `km.hour-1` converts into `m.s-1`, the kilometres into metres, the hours into seconds).
A reference converts into another one:

1. **directly**, if one of its aliases has the symbol of the other: `km = 1000 * m` into `m`;
2. **inversely**, if one of the aliases of the other has its symbol: `m` into `km`;
3. **through a common alias**, if an alias of each has the same symbol: `ft = 0.3048 * m` into `km = 1000 * m`, through
   `m`; `degF` into `degC`, through `K`.

A **main alias** takes part too: the main alias of a unit is matched with the references of the other one, so
`lbf = 4.4482216152605 * N` converts into `kg.m.s-2 (N[force])`.

FURY does not expand an alias inside a product: `lbf.s` does not convert into `kg.m.s-1`. Convert the factors instead
(see the [cookbook](/manual/cookbook#convert-a-compound-quantity)).

## When there is no conversion

| Procedure | No conversion found |
|---|---|
| `qreal%to(unit)` | an [error](./errors): the program stops |
| `uom%convert(other, magnitude, converted, is_converted)` | `is_converted` is false, `converted` is the magnitude |
| `uom%to(other, magnitude)` | returns the magnitude unchanged |

`convert` is the procedure to use when a program must handle an impossible conversion itself.

## Formulas

| Alias | From `x` in the unit to the alias symbol |
|---|---|
| `factor * symbol` | `factor * x` |
| `offset + symbol` | `offset + x` |
| `offset + factor * symbol` | `offset + factor * x` |

The inverse conversion inverts the formula. The prefixed units of a [system](./systems#prefixes) scale their aliases by
the prefix factor raised to the exponent of the symbol: `km2 = 1.e6 * m2`.

## User-supplied converters

A conversion that is not linear is supplied by a type that extends `uom_converter`, with the formulas for the three real
kinds, and an assignment:

```fortran
type, extends(uom_converter) :: dbm_to_mw
  contains
    procedure, nopass    :: convert_float128   ! real(R16P) magnitude, optional inverse
    procedure, nopass    :: convert_float64    ! real(R8P)
    procedure, nopass    :: convert_float32    ! real(R4P)
    procedure, pass(lhs) :: assign_converter   ! converter = converter
endtype dbm_to_mw
```

Each formula is a `pure` function `(magnitude, inverse) result(converted)`: it converts from the unit to the alias symbol,
or back when `inverse` is true. `convert_float128` is required also when FURY is built without quadruple precision (where
`R16P` is a double precision kind), so that the same converter compiles in both cases.

The alias of the unit is declared with `@user`, and the converter attached to it:

```fortran
dBm = uom64('dBm = @user mW')
call dBm%set_alias_conversion(reference_index=1, alias_index=2, convert=converter)
```

`reference_index` is the index of the reference in the unit (1 for a unit with a single reference), `alias_index` the
index of the alias in the reference (the main symbol is 1). [Chapter 8](/manual/tutorial/08-converters) of the tutorial
has the complete program.
