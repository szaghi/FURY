# 3. The algebra of units

Units multiply, divide and raise to powers like numbers. FURY applies the rules symbolically: the exponents of the same
symbol add up, a symbol whose exponent becomes zero disappears, and the dimensions follow.

<<< @/examples/snippets/tutorial_3-algebra.f90

## Why the references matter

A pressure can be defined by its references or by its symbol only. Both are valid, but they are not equally
informative:

<<< @/examples/snippets/tutorial_3-pressure.f90

<<< @/examples/output/tutorial_3.ansi{ansi}

From the references, a pressure times an area is `kg.m.s-2`: FURY sees that the metres cancel and the result is a force.
From the bare symbol `Pa` it can only write `Pa.m2`, correct but opaque. Define units by their references, with a main
alias for the short symbol, and the algebra works for you. The [SI system](./06-si-system) defines all its derived units
this way.

A product or a quotient has the references of its operands, never their main alias: `N` times `s` is `kg.m.s-1`, not a
newton.

::: tip What you learned
Products, quotients and powers of units; cancelling exponents; references against bare symbols.
Reference: [Units](/guide/units#algebra).
:::

Next: [4. What FURY refuses](./04-consistency).
