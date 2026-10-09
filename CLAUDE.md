# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What FURY is

FURY (Fortran Units (environment) for Reliable phYsical math) is a pure-Fortran 2018 OOP library that attaches units of measure to real quantities (`qreal`) and does symbolic algebra on unit symbols (`uom`). An inconsistent operation, such as `m + m.s-1`, is a runtime `error stop`.

## Build and test

```bash
fobis fetch                                    # deps → src/third_party/ (default branch, no pins)
fobis build --mode tests-gnu                   # all tests into exe/  (tests-gnu-debug: -fcheck=all -finit-real=nan)
bash scripts/run_tests.sh                      # runs every exe/*: PASS/FAIL by exit status, *_xfail_* must exit ≠ 0
fobis build --mode fury-static-gnu             # lib/libfury.a  (fury-shared-gnu, *-intel)
fobis build --mode converter-gnu               # exe/app/fury_converter (the only user of FLAP)
fobis rule --ex makecoverage-analysis          # what CI runs: clean, --coverage build, tests, docs/public/coverage.json
fobis rule --ex makedoc                        # formal (API from docs/ford.md) + VitePress
FC=gfortran-14 bash scripts/docs_examples.sh   # build + run docs/examples/src, regenerate snippets/outputs/images
```

- **To run a single test**, build it and execute `exe/<name>` directly. Each `src/tests/*.f90` is a standalone program.
- **FoBiS does not track `#include`d `.inc` files.** After editing one, run `fobis clean --mode <mode>`, or the change is silently not compiled. `fobis clean` leaves the executables in place, so after a rename also run `fobis rule --ex delexe`.
- **Quad precision is controlled by PENF's own macro, `PENF_R16P`.** FURY has no macro of its own: the 128-bit kinds must exist exactly when PENF's `R16P` is a distinct kind, so testing the same macro keeps the two from disagreeing. Do not reintroduce `_R16P`: it is an unprefixed, reserved-style name that pollutes the kinds of any code defining it, and PENF ≥ 2 rejects it with `#error`.
- **Dependencies are not pinned:** `fobos` and `fpm.toml` follow each repository's default branch (`master`), so every fresh fetch gets upstream HEAD. An upstream change can therefore break FURY with no FURY commit. In an existing clone, `fobis fetch --update` with no ref runs `git merge --ff-only` on the current HEAD, so a clone left on a detached commit does not move; `git -C src/third_party/<dep> checkout master` fixes it. `src/third_party/fobos.lock` is local only (gitignored). `$EXDIRS` excludes each dependency's `docs/`, `scripts/`, `src/tests` and `src/third_party`, otherwise FoBiS builds their example programs.
- Strict-standard flags are `-std=f2018` (GNU) and `-std18` (Intel): FLAP ≥ 2.5 uses F2018 features.
- PENF ≥ 2 prints reals with round-trip precision (17 significant digits for `R8P`), so golden `stringify()` strings in the tests depend on the PENF version.
- Every mode defines `PENF_R16P` except `tests-gnu-noquad` and `fury-static-gnu-noquad`. Without the macro, the `*128.F90` modules, the 128 procedures of `fury_mixed_kinds` and the 128 exports of `fury.F90` are compiled out, so `qreal128` and friends do not exist.
  - `tests-gnu-noquad` excludes the 7 tests built around `qreal128`. CI runs both configurations.
  - `uom_converter%convert_float128` stays deferred in both configurations, so user converters are source-compatible; only the generic `convert` binding is guarded.
  - fpm builds the no-quad configuration, because fpm cannot pass macros to PENF.
- New 128-only code must go behind `#ifdef PENF_R16P`, and so must new `use`/`public` statements for the 128 kinds.
- CI uses GCC 14. On this machine `gfortran` is a GCC 16 trunk build and `gcov` is GCC 14, so coverage runs locally need a matching pair.

## Architecture: one template, three kinds

The library is written once as `.inc` bodies and instantiated per real kind. For example, `fury_uom64.F90` does `use penf, RKP => R8P` and then `#include "fury_uom.inc"`; the 32 and 128 wrappers use `R4P` and `R16P`.

- **Edit the `.inc` file, not the `*32/64/128.F90` wrappers**, unless you are changing which modules a kind imports. A change to an `.inc` file hits all three kinds.
- Layering, bottom up:
  - `uom_symbol`: one symbol with exponent, factor and offset.
  - `uom_reference`: aliases plus dimensions.
  - `uom`: a product of references, and the unit algebra.
  - `qreal`: a magnitude plus a `uom`.
  - `system_abstract` / `system_si`: a registry of named units, prefixes and constants.
  - `fury_mixed_kinds` adds the operators across `qreal32/64/128`.
- `fury.F90` is the single public façade. It renames each kind's types (`qreal64`, `uom64`, `system_si64`, `system_abstract64`, …) and re-exports part of PENF. New public entities must be exported there; unsuffixed `uom`/`qreal` are **not** exported.
- `uom_symbol` holds the user converter (`class(uom_converter)`) through `type(uom_converter_box), allocatable`, not as a polymorphic allocatable component directly. F2018 C1585 forbids a pure function result having a polymorphic allocatable ultimate component, and GCC 16 enforces that. Keep the box, or every pure/elemental `uom_symbol` operator stops compiling.
- Unit grammar: `'m = meter = metre [length] {meter}'`, i.e. symbol, then `=` aliases, then `[dimensions]`, then `{name}`. Products are written `m.s-1`, and aliases can carry a factor or offset (`km = 1000.0 * m`).
- **Products and quotients** (`uom%mul`/`div`) merge references by symbol via `merge_reference`, then `remove_null_references` drops zero exponents unless *all* are zero (`N/N` stays `kg0.m0.s0`, the tests rely on it). A product never carries a main alias: copying it made `N*s` a newton and broke alias-based conversions.
- **Conversions** (`uom%convert`, the engine behind `to`): every reference must convert into a distinct reference of the target; candidates are references↔references and either unit's main alias↔the other's references (`lbf = 4.448 * N` → `kg.m.s-2 (N)`). Per reference (`uom_reference%to`): direct, inverse, then through a common alias (`ft`→`km` via `m`). `qreal%to` error-stops when nothing converts — never relabel an unconverted magnitude. No expansion of aliases inside products (`lbf.s` → `kg.m.s-1` fails by design).
- **System lookup** (`system_abstract%unit`, helpers `is_queried`/`has_synonym`): name, or a *synonym* of the main alias / single reference (same exponent, factor 1, offset 0, no converter), then the same on prefixed units. Prefixes are written symbol first, name last (`1.e3 * k = 1.e3 * kilo`): `uom%prefixed` takes the main symbol from the first alias and the name from the last. Prefixed conversion aliases scale by `factor**exponent` × the alias factor. Compound units (`m.s-1`) are not resolvable by symbol, only by name.
- The SI constants are 2019-SI exact or CODATA 2018 values; keep them sourced if you touch them.

## Error and test conventions

- Library errors write one line `error: cannot ...` to `stderr` and then `error stop 1, quiet=.true.`. Do not use plain `stop` (it exits 0, which hid failures for years). In `pure` procedures (no I/O allowed) use `error stop <message>` with the message built in a character variable.
- Regular tests end with `if (.not.all(test_passed)) error stop 1`. Expected-failure tests are named `fury_test_xfail_*` and must reach a library `error stop`.
- `fury_test_uom_aliases` is deliberately disabled: an early `stop` after setting `test_passed = .true.`.
- Generated coverage reports (`docs/guide/*.gcov.md`, `coverage-analysis.md`, `docs/public/coverage.json`) and `docs/api/` are gitignored and rebuilt by CI.

## Documentation

- VitePress in `docs/`, mirroring FLAP: `guide/` (intro, install, reference, project pages), `manual/` (9-chapter tutorial + cookbook), `api/` generated by `formal`.
- **Every code sample and output in the docs is included from `docs/examples/`** (`<<< @/examples/snippets/<prog>-<region>.f90`, `<<< @/examples/output/<id>.ansi{ansi}`). Write or change the program in `docs/examples/src/` (markers `!region`/`!endregion`, `!run [-s] ID CMD`, `!image ID`), then run `scripts/docs_examples.sh`; never hand-edit `snippets/`, `output/` or `images/`. CI (`docs-examples` job) fails if the committed files differ from a fresh run, so generate with GCC 14 (`FC=gfortran-14`) as CI does.
- `README.md`'s terminal image is `docs/examples/images/mco.svg`, rendered from the `mco` example.
