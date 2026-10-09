---
title: Errors
---

# Errors

FURY never returns a wrong number: an operation on inconsistent units prints an error on the standard error and stops
the program with `error stop`, exit status 1, so that a script or a batch system sees the failure.

## Messages

| Message | Raised by |
|---|---|
| `cannot add "A" and "B"` | `+` of quantities or units with different units |
| `cannot subtract "B" from "A"` | `-` of quantities or units with different units |
| `cannot assign "B" to "A"` | `=` of a quantity or a unit to one that has a different unit; `unset` it first |
| `cannot convert "A" to "B"` | `qreal%to` between units that no formula relates |
| `cannot raise "A" to x: the exponent would not be an integer` | a real power giving a non-integer exponent, e.g. `m ** 0.5` |
| `cannot parse "D": the exponent of the dimensions "d" differs from the one of the symbol "s"` | a definition like `Hz = s-1 [time-2]` |
| `cannot parse "D": the main alias must be one "(...)"` | a definition with more than one main alias |
| `cannot parse "D": the name must be one "{...}"` | a definition with more than one name |
| `cannot parse "D": a symbol cannot have a null multiplicative factor` | an alias like `0 * m` |

`A` and `B` are the operands, printed with their dimensions.

<<< @/examples/output/tutorial_4-add.ansi{ansi}

<<< @/examples/output/tutorial_5-impossible.ansi{ansi}

## Handling an impossible conversion

A program that must go on when a conversion does not exist uses `uom%convert`, which reports the outcome instead of
stopping (see [Conversions](./conversions#when-there-is-no-conversion)). A unit not found in a [units system](./systems)
is returned undefined: check it with `is_defined()`.

## Testing the errors

The tests of FURY named `fury_test_xfail_*` check that each inconsistency stops the program: `scripts/run_tests.sh`
counts them as passed when they exit with a non-zero status.
