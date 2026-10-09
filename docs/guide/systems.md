---
title: Units systems
---

# Units systems

A units system is a catalogue of units, prefixes and constants, queried by name. `system_si64` is the International
System of Units; `system_abstract64` is the base to build your own. Both come in 32, 64 and 128 bits.

## Using a system

| Method | Purpose |
|---|---|
| `initialize(acronym)` | builds the system (`acronym` defaults to the system's one, `SI`) |
| `unit(u)` | the unit queried, undefined if not found |
| `qunit(u)` | a quantity of magnitude 1 in the unit queried |
| `const(c)` | the constant queried, a quantity with its name |
| `list_units(with_dimensions, with_aliases, protect_aliases, with_name, compact_reals, prefix_string)` | the units, one per line |
| `list_prefixes(with_aliases, compact_reals, prefix_string)` | the prefixes, one per line |
| `list_constants(with_dimensions, with_aliases, with_name, compact_reals, prefix_string)` | the constants, one per line |
| `free()` | empties the system |

The components `acronym`, `units`, `prefixes`, `constants` and their counters are public.

### Queries

`unit(u)` returns the first unit whose

1. name is `u` (`metre`, `newton`), or
2. main alias or only reference has `u` as main symbol or synonym (`m`, `meter`, `N`, `hr`): a synonym is an alias with
   the same exponent and no factor, offset or converter, so `s` does not find the hertz, `Hz = s-1`;

first among the units of the system, then among their prefixed versions (`km`, `kilometre`, `KiB`, `kibibyte`).

### Prefixes

A prefix is a reference whose aliases carry the factor, the symbol first and the name last: `1.e3 * k = 1.e3 * kilo`. A
prefixed unit has the prefixed symbols and names (`km`, `kilometre`), and conversion aliases towards the plain unit,
scaled by the prefix factor raised to the exponent of the symbol (`km2 = 1.e6 * m2`).

## The SI system

<<< @/examples/output/si_tables.ansi{ansi}

The units are listed with their aliases, dimensions and names; the prefixes, decimal and binary (powers of 1024), with
their symbols and names; the constants with their units. The constants are exact by the definition of the 2019 SI (the
speed of light, the elementary charge, the Planck, Boltzmann and Avogadro constants, the molar gas constant), or the
CODATA 2018 values; the vacuum permittivity and impedance are derived from the permeability. `gravity` is the standard
acceleration of gravity.

### Constants

| Name | Quantity |
|---|---|
| `pi` | $\pi$, dimensionless |
| `speed_of_light` | $c$ |
| `gravity` | $g_n$, standard gravity |
| `elementary_charge` | $e$ |
| `electron_mass`, `proton_mass`, `neutron_mass` | $m_e$, $m_p$, $m_n$ |
| `planck_constant` | $h$ |
| `newton_gravitation_constant` | $G$ |
| `molar_gas_constant` | $R$ |
| `avogadro_number` | $N_A$ |
| `boltzmann_constant` | $k_B$ |
| `vacuum_permeability`, `vacuum_permittivity`, `vacuum_impedance` | $\mu_0$, $\varepsilon_0$, $Z_0$ |

## A system of your own

A system extends `system_abstract64` and implements `initialize`, with the builders:

| Method | Adds |
|---|---|
| `add_unit(source)` | a unit, from a definition string or a `uom64`; a unit equal to one already in the system is skipped |
| `add_prefix(source)` | a prefix, from a string or a `uom_reference64` |
| `add_constant(source)` | a constant, a named `qreal64` |

<<< @/examples/snippets/tutorial_9-system.f90

<<< @/examples/snippets/tutorial_9-initialize.f90

[Chapter 9](/manual/tutorial/09-own-system) of the tutorial uses it.
