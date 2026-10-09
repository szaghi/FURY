# 1. A first quantity

A FURY program uses one module, `fury`, and works with two types: a **unit of measure** (`uom64`) and a **quantity**
(`qreal64`), a real number with its unit. The `64` is the real kind: 64 bits, double precision (chapter
[7](./07-precision) has the others).

## Units from a string

A unit is defined by a string: its symbol and, between square brackets, its dimensions.

<<< @/examples/snippets/tutorial_1-define.f90

The dimensions are optional, but they make the unit algebra more informative (chapter [3](./03-unit-algebra)) and are
checked against the exponents of the symbols (chapter [4](./04-consistency)).

## Quantities

A quantity is created by the constructor `qreal64`, or by multiplying a number by a unit:

<<< @/examples/snippets/tutorial_1-quantities.f90

`R8P` is the 64 bits real kind of [PENF](https://github.com/szaghi/PENF), exported by `fury`; `9.58_R8P` is a double
precision literal. Any integer or real kind times a unit gives a quantity.

## Computing

The arithmetic operators work on quantities as on numbers, and the unit of the result is derived from the units of the
operands:

<<< @/examples/snippets/tutorial_1-compute.f90

## Printing

`stringify` returns the quantity as a string: the magnitude, then the unit. Its options choose the format of the
magnitude and what to print of the unit; the magnitude itself is the component `magnitude`.

<<< @/examples/snippets/tutorial_1-print.f90

<<< @/examples/output/tutorial_1.ansi{ansi}

- Without options the magnitude is printed with all its digits, in exponential notation.
- `format` is a Fortran edit descriptor for the magnitude.
- `compact_reals=.true.` prints the shortest representation of the magnitude.
- `with_dimensions=.true.` appends the dimensions, derived like the symbols: `length.time-1`.
- `speed * time` is a length again: `m.s-1` times `s` is `m`.

::: tip What you learned
Units from strings, quantities by constructor or by `number * unit`, arithmetic with derived units, `stringify`.
Reference: [Quantities](/guide/quantities), [Units](/guide/units).
:::

Next: [2. Defining units](./02-defining-units).
