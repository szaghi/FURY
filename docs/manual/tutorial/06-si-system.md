# 6. The SI system

Defining every unit by hand is tedious and error prone: `system_si64` is a catalogue of the International System of
Units, ready to use. It is initialised once:

<<< @/examples/snippets/tutorial_6-initialize.f90

## Units by name or symbol

`unit` returns a unit of the system, queried by name, by symbol or by the main alias of a compound unit:

<<< @/examples/snippets/tutorial_6-units.f90

A query matches the name of a unit (`metre`), its symbol (`m`), a synonym of its symbol (`meter`, `sec`) or its main
alias (`N`), and also the units obtained prefixing the system units (`km`, `kilometre`):

<<< @/examples/snippets/tutorial_6-queries.f90

## Prefixes

The prefixed units are built on demand, with their conversion to the plain unit:

<<< @/examples/snippets/tutorial_6-prefixed.f90

`qunit` returns a quantity of magnitude 1 in the queried unit: a number times it is a quantity. The binary prefixes are
powers of 1024:

<<< @/examples/snippets/tutorial_6-qunit.f90

## Constants

`const` returns a physical constant, as a quantity with its unit and name:

<<< @/examples/snippets/tutorial_6-constants.f90

<<< @/examples/output/tutorial_6.ansi{ansi}

The weight of a 75 kg person is a mass times an acceleration: a force, `kg.m.s-2`. The [Units systems](/guide/systems)
page lists all the units, prefixes and constants of the SI system.

::: tip What you learned
`initialize`, `unit` by name, symbol, synonym or prefix, `qunit`, `const`.
Reference: [Units systems](/guide/systems).
:::

Next: [7. Precision](./07-precision).
