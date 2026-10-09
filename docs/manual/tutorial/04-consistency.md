# 4. What FURY refuses

FURY checks the units at every operation that needs them consistent, and **stops the program** at the first
inconsistency: it prints an error on the standard error and ends with `error stop`, exit status 1. A wrong number never
leaves the operation.

The program of this chapter takes a scenario from the command line; the quantities are

```fortran
length       = 3._R8P * metre
other_length = 2._R8P * metre
time         = 2._R8P * second
```

## Sums and differences

A sum, or a difference, needs the same units on both sides:

<<< @/examples/snippets/tutorial_4-add.f90

<<< @/examples/output/tutorial_4-add.ansi{ansi}

Products and quotients are always allowed: their unit is derived (chapter [3](./03-unit-algebra)).

## Assignments

A quantity that has a unit keeps it: assigning it a quantity with another unit is an error.

<<< @/examples/snippets/tutorial_4-assign.f90

<<< @/examples/output/tutorial_4-assign.ansi{ansi}

The first assignment gives `total` the unit `m`; the second one would silently change it into a time. To reuse a variable
for another unit, `unset` it first:

<<< @/examples/snippets/tutorial_4-reuse.f90

<<< @/examples/output/tutorial_4-reuse.ansi{ansi}

The same holds for units: a `uom64` that has a unit is assigned only the same unit, unless it is `unset`.

## Definitions

The dimensions of a symbol must have its exponent: a frequency in `s-1` has dimensions `time-1`, not `time-2`.

<<< @/examples/snippets/tutorial_4-parse.f90

<<< @/examples/output/tutorial_4-parse.ansi{ansi}

## Comparisons

`==` and `/=` compare the magnitudes and the units; `has_same_unit` compares the units only:

<<< @/examples/snippets/tutorial_4-compare.f90

<<< @/examples/output/tutorial_4-compare.ansi{ansi}

::: tip What you learned
Sums, differences, assignments and definitions are checked; an inconsistency stops the program with exit status 1;
`unset` frees a variable for another unit.
Reference: [Errors](/guide/errors).
:::

Next: [5. Conversions](./05-conversions).
