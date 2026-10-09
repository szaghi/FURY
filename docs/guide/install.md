---
title: Installation
---

# Installation

FURY needs a Fortran 2018 compiler: it is tested with gfortran 14 and 16. Its dependencies, all by the same author, are
fetched by the build system:

| Library | Purpose |
|---|---|
| [PENF](https://github.com/szaghi/PENF) | portable kind parameters (`R4P`, `R8P`, `R16P`, ...) and number-to-string conversion |
| [StringiFor](https://github.com/szaghi/StringiFor) | strings, used by the parser of the unit definitions |
| [BeFoR64](https://github.com/szaghi/BeFoR64), [FACE](https://github.com/szaghi/FACE) | dependencies of StringiFor |
| [FLAP](https://github.com/szaghi/FLAP) | command line parsing, used only by the `fury_converter` app |

## FoBiS

[FoBiS](https://github.com/szaghi/FoBiS) is the build system FURY is developed with (`pip install FoBiS.py`).

**Standalone**: clone, fetch the dependencies, build.

```bash
git clone https://github.com/szaghi/FURY && cd FURY
fobis fetch                            # dependencies into src/third_party/
fobis build --mode fury-static-gnu     # lib/libfury.a and lib/mod/
```

| Mode | Builds |
|---|---|
| `fury-static-gnu`, `fury-shared-gnu` | `lib/libfury.a`, `lib/libfury.so` with gfortran |
| `fury-static-intel`, `fury-shared-intel` | the same with Intel Fortran |
| `fury-static-gnu-noquad` | `lib/libfury.a` without the 128 bits kinds |
| `tests-gnu`, `tests-gnu-debug`, `tests-gnu-noquad` | the tests, into `exe/` |
| `converter-gnu`, `converter-intel` | the `fury_converter` app, into `exe/app/` |

`fobis build --lmodes` lists them all. To use the library, link `libfury` and add `lib/mod` to the module search path.

**As a project dependency**: declare FURY in your `fobos` and fetch it.

```ini
[dependencies]
deps_dir = src/third_party
FURY     = https://github.com/szaghi/FURY
```

The dependencies follow the head of their default branch (`master`).

## fpm

```toml
[dependencies]
FURY = { git = "https://github.com/szaghi/FURY" }
```

fpm cannot pass a preprocessor macro to a dependency, so PENF is built without quadruple precision and FURY with it:
FURY built by fpm has no 128 bits kinds (see [Precision](./precision)).

## Quadruple precision

The 128 bits kinds (`qreal128`, `uom128`, `system_si128`, ...) exist only when FURY and PENF are compiled with
`-DPENF_R16P`, as all the FoBiS modes but the `*-noquad` ones do. FURY tests PENF's own macro: the two cannot disagree on
whether `R16P` is a quadruple precision kind.

## Tests

```bash
fobis build --mode tests-gnu
bash scripts/run_tests.sh
```

A test passes when it exits with status 0; the tests named `*_xfail_*` check that an inconsistency stops the program,
and pass when they exit with a non-zero status. `bash scripts/docs_examples.sh` builds and runs the examples of this
documentation.

## The converter app

`fury_converter` converts a value between two units of the SI system:

```bash
fobis build --mode converter-gnu
./exe/app/fury_converter --input_uom km --output_uom m --value 3.2
```
