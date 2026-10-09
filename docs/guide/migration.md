---
title: Upgrading
---

# Upgrading

## To the next release (from 0.8.x)

This release fixes the build with current compilers and several wrong results, and changes some behaviours. A program
written for 0.8.x needs these changes:

| Change | What to do |
|---|---|
| Quadruple precision is controlled by PENF's macro `PENF_R16P`; `_R16P_SUPPORTED` is gone | compile with `-DPENF_R16P`; without it the 128 bits types do not exist |
| `fury.f90` is renamed `fury.F90` | update a build that names it |
| A Fortran 2018 compiler mode is required (`-std=f2018`) | |
| The dependencies are fetched by `fobis fetch` (or fpm), no more git submodules | run `git submodule deinit -f --all`, then `fobis fetch` |
| An inconsistency stops with `error stop` (exit status 1), it used `stop` (exit status 0) | scripts can rely on the exit status |
| `qreal%to` stops if the conversion does not exist; it returned the magnitude unchanged, labelled with the new unit | use `uom%convert` to handle the case |
| A real power giving a non-integer exponent stops; `m ** 0.5` was silently `m0` | |
| The error messages are reworded, one line each | update tests that match them |
| `stringify` of reals follows PENF 2: round-trip precision, e.g. `+0.10438413361169102E+002` | update golden outputs, or print with `format` |
| A unit without dimensions prints no `[]` | |

## Fixed

- Unit products and quotients: the last reference of the right operand was skipped; the main alias of the left operand
  was kept (`N` times `s` was a newton); cancelling exponents were kept (`m.s0`).
- Conversions: a conversion into a compound unit through its main alias (`lbf` into `kg.m.s-2 (N)`) and through a common
  alias (`ft` into `km`) are found; a conversion is accepted only if every reference is converted.
- Prefixes: the alias factors were dropped (`kbyte = kbit`); the prefix factor ignored the exponent (`km2 = 1000 * m2`).
- SI system: binary prefixes are powers of 1024 (they were `2.e10`, ...); deca is `da`, not `d` like deci; the prefixed
  units are `km`, not `kilom`; lumen was dropped as a duplicate of the candela; exact imperial units; constants of the
  2019 SI and CODATA 2018.
- SI queries: synonyms (`meter`, `hr`, `min`) and prefixed symbols (`km`, `KiB`) are found.
- The symbol getters returned undefined values for an undefined symbol, which made the definitions without dimensions
  fail with some compilers.

## New

- `uom%convert`, conversion reporting its outcome; `/=` for units; the `system_abstract*` types exported by `fury`, to
  build systems of your own.
