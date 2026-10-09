---
title: Cookbook
---

# Cookbook

Short answers to "how do I ...?". Each recipe shows the code and its real output; the [reference](/guide/features) has
the details.

[[toc]]

## Attach a unit to a number

<<< @/examples/snippets/cookbook_attach-attach.f90

<<< @/examples/output/cookbook_attach.ansi{ansi}

The constructor also names the quantity; `number * unit` works with any integer or real kind; `qunit` takes the unit
from a [units system](/guide/systems).

## Print a quantity

<<< @/examples/snippets/cookbook_print-print.f90

<<< @/examples/output/cookbook_print.ansi{ansi}

`with_aliases` prints the aliases of the symbols and the main alias, `with_name` the name of the quantity and of its
unit. `format` takes any Fortran edit descriptor for reals.

## Sum lengths given in different units

<<< @/examples/snippets/cookbook_sum-sum.f90

<<< @/examples/output/cookbook_sum.ansi{ansi}

Convert first, then sum: a sum of different units [stops the program](/manual/tutorial/04-consistency#sums-and-differences).

## Convert temperatures

<<< @/examples/snippets/cookbook_temperature-temperature.f90

<<< @/examples/output/cookbook_temperature.ansi{ansi}

Fahrenheit and Celsius are both defined towards the kelvin, with an offset and a factor: FURY converts between them
through `K`.

## Compute with the SI system

The kinetic energy of a car, from a speed in km/h:

<<< @/examples/snippets/cookbook_energy-energy.f90

<<< @/examples/output/cookbook_energy.ansi{ansi}

`km.hour-1` is converted reference by reference, the kilometres into metres and the hours into seconds; the energy,
`kg.m2.s-2`, is converted into the joule to print its main alias.

## Powers and roots

<<< @/examples/snippets/cookbook_powers-powers.f90

<<< @/examples/output/cookbook_powers.ansi{ansi}

A real exponent is allowed when the resulting exponents of the symbols are integers.

## Convert a compound quantity

FURY converts a unit symbol by symbol, and does not expand an alias inside a product: an impulse in `lbf.s` is not
converted into `kg.m.s-1` directly. Convert the force, then multiply:

<<< @/examples/snippets/cookbook_impulse-impulse.f90

<<< @/examples/output/cookbook_impulse.ansi{ansi}

## Compare quantities

<<< @/examples/snippets/tutorial_4-compare.f90

<<< @/examples/output/tutorial_4-compare.ansi{ansi}

`==` and `/=` compare the magnitudes and the units, `has_same_unit` the units only. Comparing floating point magnitudes
with `==` has the usual pitfalls: compare `abs(a%magnitude - b%magnitude)` with a tolerance when the values come from
computations.

## Reuse a variable for another unit

<<< @/examples/snippets/tutorial_4-reuse.f90

<<< @/examples/output/tutorial_4-reuse.ansi{ansi}

Without `unset`, the second assignment [stops the program](/manual/tutorial/04-consistency#assignments).

## Look up a unit of the SI system

<<< @/examples/snippets/tutorial_6-queries.f90

The query matches a name, a symbol, a synonym, a main alias or a prefixed unit; a unit not found is returned undefined
(`is_defined()` is false).

## Use a physical constant

<<< @/examples/snippets/tutorial_6-constants.f90

The [Units systems](/guide/systems#constants) page lists the constants of the SI system.

## Data sizes

<<< @/examples/snippets/tutorial_6-qunit.f90

The binary prefixes (`Ki`, `Mi`, `Gi`, ...) are powers of 1024, the decimal ones (`k`, `M`, `G`, ...) powers of 1000.

## A conversion that is not a factor

A user-supplied converter, from dBm to mW:

<<< @/examples/snippets/tutorial_8-use.f90

<<< @/examples/output/tutorial_8.ansi{ansi}

The converter itself is in [chapter 8](/manual/tutorial/08-converters).

## Mix precisions

<<< @/examples/snippets/tutorial_7-mixed.f90

The result of a mixed-kind operation has the kind of the more precise operand.

## Catch the Mars Climate Orbiter bug

<<< @/examples/snippets/mco.f90

<<< @/examples/output/mco.ansi{ansi}

## Stop instead of a wrong conversion

<<< @/examples/snippets/tutorial_5-impossible.f90

<<< @/examples/output/tutorial_5-impossible.ansi{ansi}

## Define a units system

<<< @/examples/snippets/tutorial_9-initialize.f90

The complete program is [chapter 9](/manual/tutorial/09-own-system).
