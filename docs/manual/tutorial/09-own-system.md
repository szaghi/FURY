# 9. A units system of yours

The SI system is one `system_abstract`; you can build your own, with the units, prefixes and constants of your domain.
A system extends `system_abstract64` (or `32`, `128`) and implements `initialize`:

<<< @/examples/snippets/tutorial_9-system.f90

`initialize` frees the system, then adds its units and prefixes from strings, in the [grammar](/guide/grammar) of the
previous chapters, and its constants as quantities:

<<< @/examples/snippets/tutorial_9-initialize.f90

A prefix lists its symbol first and its name last (`k = kilo`): the prefixed units take the symbol in their symbol (`km`)
and the name in their name (`kilometre`).

## Using it

<<< @/examples/snippets/tutorial_9-use.f90

<<< @/examples/output/tutorial_9.ansi{ansi}

`list_units` lists the units of the system. The cruise altitude, in feet, is converted into kilometres: `ft` and `km`
share no formula, but both are defined through `m`, and FURY converts through it.

::: tip What you learned
A system extends `system_abstract` and implements `initialize` with `add_unit`, `add_prefix`, `add_constant`; units are
converted through the aliases they share.
Reference: [Units systems](/guide/systems#a-system-of-your-own).
:::

The tutorial ends here. The [cookbook](../cookbook) has short recipes for everyday tasks, and the
[reference](/guide/features) has the details of everything you met.
