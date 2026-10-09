# 5. Conversions

An alias of a unit can be a **conversion formula**: a factor, an offset, or both. `to` converts a quantity into another
unit through these formulas.

## A factor

<<< @/examples/snippets/tutorial_5-factor.f90

<<< @/examples/output/tutorial_5-factor.ansi{ansi}

`km = 1000 * m` reads "one km is 1000 m". The conversion works both ways: `to` uses the formula of either unit.

## Factors with a decimal point

The `.` separates the references of a compound unit, so a factor with a decimal point must be **protected** between
`<` and `>`:

<<< @/examples/snippets/tutorial_5-protected.f90

<<< @/examples/output/tutorial_5-protected.ansi{ansi}

## An offset

`offset + factor * symbol` is a linear formula with an offset, for the temperature scales:

<<< @/examples/snippets/tutorial_5-offset.f90

<<< @/examples/output/tutorial_5-offset.ansi{ansi}

## Several aliases

A unit can have several aliases: synonyms (`hr`) and formulas towards several units. The hour is a symbol and a formula
at once:

<<< @/examples/snippets/tutorial_5-aliases.f90

<<< @/examples/output/tutorial_5-aliases.ansi{ansi}

When neither unit has a formula towards the other, FURY looks for an alias they share: `ft = 0.3048 * m` and
`km = 1000 * m` convert into each other through `m` (chapter [9](./09-own-system) has an example).

## Through the main alias

The pound-force is defined by a formula towards `N`, the main alias of the newton. FURY matches it, and converts a
single symbol into a compound unit:

<<< @/examples/snippets/tutorial_5-main-alias.f90

<<< @/examples/output/tutorial_5-main-alias.ansi{ansi}

The converted quantity has the unit passed to `to`: the newton, `kg.m.s-2`, with its main alias `N`.

## No conversion, no number

A conversion between units that no formula relates stops the program, like a sum: `to` never returns the magnitude with a
unit it does not have.

<<< @/examples/snippets/tutorial_5-impossible.f90

<<< @/examples/output/tutorial_5-impossible.ansi{ansi}

FURY converts a unit symbol by symbol (or through the main alias): it does not expand an alias inside a product. An
impulse in `lbf.s` is not converted into `kg.m.s-1` directly; the [cookbook](/manual/cookbook#convert-a-compound-quantity)
shows how to convert its force first.

::: tip What you learned
Factors, protected factors, offsets, several aliases, conversions through a common alias and through the main alias;
an impossible conversion stops the program.
Reference: [Conversions](/guide/conversions), [Unit grammar](/guide/grammar#aliases).
:::

Next: [6. The SI system](./06-si-system).
