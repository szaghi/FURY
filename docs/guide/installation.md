# Installation

## Prerequisites

A Fortran 2008 compiler and [FoBiS](https://github.com/szaghi/FoBiS):

```bash
pip install FoBiS.py
```

## Download

```bash
git clone https://github.com/szaghi/FURY
cd FURY
fobis fetch
```

`fobis fetch` clones the dependencies declared in the `fobos` file, at the head of their default branch, into
`src/third_party/`:

| Library | Purpose |
|---------|---------|
| [PENF](https://github.com/szaghi/PENF) | portable kind parameters and number-to-string conversion |
| [StringiFor](https://github.com/szaghi/StringiFor) | strings, used by the unit parser |
| [BeFoR64](https://github.com/szaghi/BeFoR64), [FACE](https://github.com/szaghi/FACE) | StringiFor dependencies |
| [FLAP](https://github.com/szaghi/FLAP) | command line parsing, used only by the `fury_converter` app |

## Quadruple precision

The 128-bit kinds (`qreal128`, `uom128`, `system_si128`, ...) exist only when FURY and PENF are compiled with
`-DPENF_R16P` (the PENF macro, so FURY and PENF always agree), as all the FoBiS modes do. Without it (the `*-noquad` FoBiS modes, or fpm) FURY provides only the 32 and
64-bit kinds, and the 128-bit types are not defined at all. A user-supplied `uom_converter` must implement
`convert_float128` in both cases.

## Build with fpm

```toml
[dependencies]
FURY = { git = "https://github.com/szaghi/FURY" }
```

fpm cannot pass `-DPENF_R16P` to PENF, so FURY built by fpm has no 128-bit kinds.

## Build the library

```bash
fobis build --mode fury-static-gnu    # lib/libfury.a
fobis build --mode fury-shared-gnu    # lib/libfury.so
fobis build --mode fury-static-gnu-noquad   # without the 128-bit kinds
```

The Intel Fortran modes are `fury-static-intel` and `fury-shared-intel`; `fobis build --lmodes` lists all modes.
Link `libfury` and add `lib/mod` to the module search path of your project.

## Build and run the tests

```bash
fobis build --mode tests-gnu          # or tests-gnu-debug
bash scripts/run_tests.sh
```

Every test is a program in `src/tests/`, compiled into `exe/`. A test passes when it exits with status 0; the tests
named `*_xfail_*` check that an inconsistent operation stops the program, and pass when they exit with a non-zero
status.

## Build the converter app

```bash
fobis build --mode converter-gnu
./exe/app/fury_converter --input_uom metre --output_uom m --value 2
```
