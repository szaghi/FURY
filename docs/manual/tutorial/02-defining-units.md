# 2. Defining units

The string that defines a unit can say much more than a symbol. Each part is optional but the first one, and each adds
information that FURY uses:

<<< @/examples/snippets/tutorial_2-define.f90

| Part | Example | Meaning |
|---|---|---|
| symbol | `m` | the main symbol, the only required part |
| `= alias` | `= metre = meter` | other symbols of the same unit (synonyms), or conversion formulas (chapter [5](./05-conversions)) |
| `[dimensions]` | `[length]` | the dimensions of the symbol, with the same exponent |
| `.` | `kg.m.s-2` | a product of reference units, each with its exponent and dimensions |
| `(alias[dimensions])` | `(N[force])` | the **main alias** of a compound unit: the symbol it is known by |
| `{name}` | `{newton}` | the name of the unit, used to query a [units system](./06-si-system) |

`stringify` prints the parts it is asked for:

<<< @/examples/snippets/tutorial_2-print.f90

<<< @/examples/output/tutorial_2.ansi{ansi}

## Compound units

A compound unit is a product of **reference units** separated by `.`, each with an integer exponent: `kg.m.s-2` is the
kilogram times the metre times the second to the power -2, the notation of
[The Unified Code for Units of Measure](https://ucum.org/). Each reference has its own dimensions,
`kg [mass].m [length].s-2 [time-2]`, and FURY combines them: `mass.length.time-2`.

The **main alias** in parentheses is the short symbol of a compound unit, `N` for `kg.m.s-2`, with its own dimensions.
It is how the unit is printed with `with_aliases=.true.`, and FURY uses it to convert the unit
([chapter 5](./05-conversions)).

::: tip What you learned
The parts of a unit definition: symbol, aliases, dimensions, compound units, main alias, name.
Reference: [Unit grammar](/guide/grammar).
:::

Next: [3. The algebra of units](./03-unit-algebra).
