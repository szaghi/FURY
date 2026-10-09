---
title: Introduction
---

# Introduction

FURY (Fortran Units (environment) for Reliable phYsical math) is a pure Fortran library for physical computations that
check their units of measure. A FURY **quantity** is a number together with its **unit**: the operators of the language
work on quantities as on numbers, and FURY checks and propagates the units at every step.

<<< @/examples/snippets/bolt-quantities.f90

<<< @/examples/output/bolt.ansi{ansi}

The quotient of a length by a time is a speed: its unit `m.s-1` and its dimensions `length.time-1` are derived by FURY,
not written by hand. Add a length to that speed, assign it to a time, convert it to kilograms, and the program stops with
an error that says what is wrong.

## Why

On 23 September 1999 the Mars Climate Orbiter was lost entering the Martian atmosphere. The investigation board found the
root cause in a ground software file, `SM_FORCES`, that produced the impulses of the thrusters in **pound-force seconds**,
where the navigation software expected **newton seconds**: every correction of the trajectory was underestimated by a
factor of 4.45. The two numbers were plausible, the programs ran, and nothing checked the units.

FURY makes that check part of the arithmetic. The same mistake, written with FURY:

<<< @/examples/snippets/mco.f90

<<< @/examples/output/mco.ansi{ansi}

The program does not produce a wrong trajectory: it stops at the sum, with a non-zero exit status that a script or a
batch system sees. The [Background](./background) page tells more about the question behind FURY.

## The concepts

| Concept | Type | What it is |
|---|---|---|
| Unit of measure | `uom64` | a product of reference units, `kg.m.s-2`, each with its symbol, aliases and dimensions |
| Quantity | `qreal64` | a real magnitude and its unit: `9.81 m.s-2` |
| Units system | `system_si64` | a catalogue of units, prefixes and constants queried by name: `SI%unit('newton')` |
| Converter | `uom_converter` | a formula of your own for conversions that are not a factor and an offset |

Every type comes in three real kinds, suffixed `32`, `64` and `128` (see [Precision](./precision)).

## Reading this documentation

The pages read in order, and each one links to the next:

1. [Installation](./install): get FURY into your project.
2. The [tutorial](/manual/): nine short chapters, from a first quantity to a units system of your own.
3. The [cookbook](/manual/cookbook): short recipes, one for each "how do I ...?".
4. The reference, from the [feature map](./features) on: the unit grammar, every type, every error.

The [API](/api/) documents the source itself. Upgrading from an older release? See [Upgrading](./migration).

Every code sample of this documentation is part of a program that is compiled and run to produce the outputs shown (see
[`docs/examples`](https://github.com/szaghi/FURY/tree/master/docs/examples)).

## Authors

- Stefano Zaghi — [@szaghi](https://github.com/szaghi)

FURY is dedicated to W. Van Snyder. Contributions are welcome — see the [Contributing](./contributing) page.

## Copyrights

FURY is distributed under a multi-licensing system:

| Use case | License |
|---|---|
| FOSS projects | [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html) |
| Closed source / commercial | [BSD 2-Clause](http://opensource.org/licenses/BSD-2-Clause) |
| Closed source / commercial | [BSD 3-Clause](http://opensource.org/licenses/BSD-3-Clause) |
| Closed source / commercial | [MIT](http://opensource.org/licenses/MIT) |
