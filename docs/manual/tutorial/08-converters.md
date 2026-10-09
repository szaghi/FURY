# 8. Non-linear conversions

Factors and offsets cover most conversions, but not all: the power in decibel-milliwatts is a logarithm of the power in
milliwatts, $P_{dBm} = 10 \log_{10} P_{mW}$. For such conversions FURY takes a **converter** of your own: a type that
extends `uom_converter` with the formulas.

<<< @/examples/snippets/tutorial_8-converter.f90

The formulas convert a magnitude from the unit to its alias, or back with `inverse=.true.`; there is one for each real
kind, so that the same converter serves `uom32`, `uom64` and `uom128` (the 128 bits one is required even without
quadruple precision). The 64 bits one:

<<< @/examples/snippets/tutorial_8-formulas.f90

`assign_converter` copies a converter (see the [full program](https://github.com/szaghi/FURY/tree/master/docs/examples/src/tutorial_8.f90)).

## Using it

The alias `@user mW` declares a conversion towards `mW` whose formula is supplied by the program; `set_alias_conversion`
attaches the converter to it, by the index of the reference (1) and of the alias (2, the first is the symbol `dBm`):

<<< @/examples/snippets/tutorial_8-use.f90

<<< @/examples/output/tutorial_8.ansi{ansi}

::: tip What you learned
A converter extends `uom_converter`; `@user` declares the alias, `set_alias_conversion` attaches the formulas.
Reference: [Conversions](/guide/conversions#user-supplied-converters).
:::

Next: [9. A units system of yours](./09-own-system).
