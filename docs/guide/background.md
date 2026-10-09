---
title: Background
---

# Background

## Reliability counts

Reliability is the consistency of a set of measures: the probability that a computation succeeds, one minus the
probability of a failure. A wrong unit is a failure that no compiler sees: the numbers are plausible, the program runs,
and the result is wrong.

FURY started from two questions, raised in a
[comp.lang.fortran discussion](https://groups.google.com/forum/#!topic/comp.lang.fortran/1TbfQlAmKx8) started by
W. Van Snyder, to whom FURY is dedicated:

1. Is a **units-consistency-check facility** desirable for a programming language? Useful, or dangerous because it adds
   more room for errors than it removes?
2. Can it be implemented in Fortran?

FURY is an answer to the second question, and an argument for the first one: a units-consistency-check facility is
feasible in standard Fortran, and the experience of other languages (below) shows that it is useful.

## The Mars Climate Orbiter

The Mars Climate Orbiter Mishap Investigation Board found the root cause of the loss of the spacecraft, on 23 September
1999, in the failure to use metric units in a ground software file, `SM_FORCES` ("small forces"), used by the trajectory
models:

> thruster performance data in English units instead of metric units was used in the software application code titled
> SM_FORCES (small forces).

The impulses were produced in pound-force seconds where newton seconds were expected: the trajectory was underestimated
by a factor of 4.45, the conversion factor from pounds-force to newtons. A units-consistency-check facility would have
stopped the program at the first sum: see the [introduction](./).

## References

- [The Unified Code for Units of Measure](https://ucum.org/)
- S. Meyers, *Dimensional analysis*, C++ Report
- G. W. Petty, *Automated computation and consistency checking of physical dimensions and units in scientific programs*,
  Software: Practice and Experience, 2001
- W. E. Brown, *Introduction to the SI Library of Unit-Based Computation*, Fermilab, 1998
- [International System of Units](https://en.wikipedia.org/wiki/International_System_of_Units)

## Units in other languages

| Language | Library |
|---|---|
| F# | [units of measure](https://learn.microsoft.com/dotnet/fsharp/language-reference/units-of-measure), built in the language |
| .NET | [UnitsNet](https://github.com/angularsen/UnitsNet) |
| Ada | [Units](http://www.dmitry-kazakov.de/ada/units.htm) |
| C++ | [Boost.Units](https://www.boost.org/doc/libs/release/libs/units/), [mp-units](https://github.com/mpusz/mp-units) |
| Haskell | [dimensional](https://hackage.haskell.org/package/dimensional) |
| JavaScript | [ucum.js](https://github.com/jmandel/ucum.js) |
| Julia | [Unitful](https://github.com/PainterQubits/Unitful.jl) |
| Python | [pint](https://github.com/hgrecco/pint), [quantities](https://github.com/python-quantities/python-quantities) |
| Ruby | [unitwise](https://github.com/joshwlewis/unitwise) |
| Rust | [uom](https://github.com/iliekturtles/uom) |
