<a name="top"></a>

# FURY [![GitHub tag](https://img.shields.io/github/tag/szaghi/FURY.svg)]() [![Join the chat at https://gitter.im/szaghi/FURY](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/szaghi/FURY?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[![License](https://img.shields.io/badge/license-GNU%20GeneraL%20Public%20License%20v3,%20GPLv3-blue.svg)]()
[![License](https://img.shields.io/badge/license-BSD2-red.svg)]()
[![License](https://img.shields.io/badge/license-BSD3-red.svg)]()
[![License](https://img.shields.io/badge/license-MIT-red.svg)]()

[![Status](https://img.shields.io/badge/status-unstable-red.svg)]()
[![Build Status](https://travis-ci.org/szaghi/FURY.svg?branch=master)](https://travis-ci.org/szaghi/FURY)
[![Coverage Status](https://img.shields.io/codecov/c/github/szaghi/FURY.svg)](http://codecov.io/github/szaghi/FURY?branch=master)

### FURY, Fortran Units (environment) for Reliable phYsical math

A KISS pure Fortran Library for improving physical math computations by taking into account units of measure:

- FURY is a pure Fortran (KISS) library for improving physical math computations;
- FURY is Fortran 2003+ standard compliant;
- FURY is OOP designed;
- FURY is a Free, Open Source Project.

#### Issues

[![GitHub issues](https://img.shields.io/github/issues/szaghi/FURY.svg)]()
[![Ready in backlog](https://badge.waffle.io/szaghi/FURY.png?label=ready&title=Ready)](https://waffle.io/szaghi/FURY)
[![In Progress](https://badge.waffle.io/szaghi/FURY.png?label=in%20progress&title=In%20Progress)](https://waffle.io/szaghi/FURY)
[![Open bugs](https://badge.waffle.io/szaghi/FURY.png?label=bug&title=Open%20Bugs)](https://waffle.io/szaghi/FURY)

#### Compiler Support

[![Compiler](https://img.shields.io/badge/GNU-v5.3.x+-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/Intel-v16.x+-brightgreen.svg)]()
[![Compiler](https://img.shields.io/badge/IBM%20XL-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/g95-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/NAG-not%20tested-yellow.svg)]()
[![Compiler](https://img.shields.io/badge/PGI-not%20tested-yellow.svg)]()

---

[What is FURY?](#what-is-FURY) | [Main features](#main-features) | [Copyrights](#copyrights) | [Download](#download) | [Compilation](#compilation) | [Documentation](#documentation)

---

## What is FURY?

An inspiring [Google Group discussion](https://groups.google.com/forum/#!topic/comp.lang.fortran/1TbfQlAmKx8) pointed out that

> it could be useful to take into account *units of measure* in order to improve *computations reliability*.

This discussion showed that the topic is really *controversial*: someone want *units check facility* to be a built-in feature in Fortran, others claim that such a feature is not useful at all or even dangerous just because it could add more space for errors than it could reduce (thus reducing the reliability). I am in a *limbo*: physical units checking is a feature that I do not need because I am used to think to non dimensional quantities, but all features that could improve Fortran are able to catch my curiosity. As a consequence, with the only purpose to satisfy my insane curiosity, I am starting the development of this library to check if units checking is:

+ feasible;
+ useful... or
+ dangerous!

The feasibility is related to current Fortran limitations: I have to play with derived types, no other simple ways are viable. However, this means that all the computations that we usually do with integer/complex/real variables must be done with new derived types (that I will design as fully OO classes). As a consequence, feasibility with the current Fortran standard is not obvious for me.

The usefulness is my main concern: as stated, I prefer to think in non dimensional quantities, normalized in order to minimize the always present truncations errors due to the computer finite precision. However, in other scenario (with which I am not familiar) it could be impossible to adopt non dimensional math, thus units checking could matter. Moreover, even in my fortunate non dimensional world, there is a situation where unit checking could help: the input of physical data is often than in dimensional quantities and only before the computations these data are normalized to become non dimensional: this phase is easy to check to be *units consistent* even without a *units checking facility*, but a *small reliability improvement* is ever an improvement.

The dangerous nature of the *units checking facility* is out the scope of this project: the curious readers are kindly invited to study this aspect are report here their results :smile:

Go to [Top](#top)

## Main features

FURY is inspired by the python great module [pint](https://github.com/hgrecco/pint), thus many features are taken from it. Here the main features are listed.

* [ ] User-friendly classes to add *units of measure* to numbers;
* [ ] errors trapping for invalid computations;
* [ ] unit parsing: prefixed and pluralized forms of units are recognized without explicitly defining them, i.e. as the prefix kilo and the unit meter are defined, FURY understands kilometers;
* [ ] standalone unit definitions: units definitions are loaded from simple and easy to edit text file;
* [ ] advanced string formatting;
* [ ] replicate all the useful features of [pint](https://github.com/hgrecco/pint);

Any feature request is welcome.

Go to [Top](#top)

## Copyrights

FURY is an open source project, it is distributed under a multi-licensing system:

+ for FOSS projects:
  - [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html);
+ for closed source/commercial projects:
  - [BSD 2-Clause](http://opensource.org/licenses/BSD-2-Clause);
  - [BSD 3-Clause](http://opensource.org/licenses/BSD-3-Clause);
  - [MIT](http://opensource.org/licenses/MIT).

Anyone is interest to use, to develop or to contribute to FURY is welcome, feel free to select the license that best matches your soul!

More details can be found on [wiki](https://github.com/szaghi/FURY/wiki/Copyrights).

Go to [Top](#top)

## Download

FURY home is at [https://github.com/szaghi/FURY](https://github.com/szaghi/FURY). It uses a bunch of bash scripts to handle the third party dependencies. To download all the source files you can:

+ clone recursively this repository: `git clone --recursive https://github.com/szaghi/FURY`
+ download the latest master-branch archive at [https://github.com/szaghi/FURY/archive/master.zip](https://github.com/szaghi/FURY/archive/master.zip)
+ download a release archive at [https://github.com/szaghi/FURY/releases](https://github.com/szaghi/FURY/releases)

Go to [Top](#top)

## Compilation

FURY is a modern Fortran project thus a modern Fortran compiler is need to compile the project. In the following table the support for some widely-used Fortran compilers is summarized.

The library is modular, namely it exploits Fortran modules. As a consequence, there is compilation-cascade hierarchy to build the library. To correctly build the library the following approaches are supported

+ [Build by means of FoBiS](#build-by-means-of-fobis): full support;
+ [Build by means of GNU Make](#build-by-means-of-gnu-make): to be implemented.
+ [Build by means of CMake](#build-by-means-of-cmake): to be implemented.

The FoBiS building support is the most complete, as it is the one used for the developing FURY.

### Build by means of FoBiS

A `fobos` file is provided to build the library by means of the Fortran Building System [FoBiS](https://github.com/szaghi/FoBiS).

#### Build all tests

Type

```shell
FoBiS.py build
```

After (a successful) building a directory `./exe` is created containing all the compiled tests that constitute the FURY *regression-tests-suite*, e.g.

```bash
→ FoBiS.py build
Builder options
Directories
  Building directory: "exe"
  Compiled-objects .o   directory: "exe/obj"
  Compiled-objects .mod directory: "exe/mod"
Compiler options
  Vendor: "gnu"
  Compiler command: "gfortran"
  Module directory switch: "-J"
  Compiling flags: "-c -frealloc-lhs -std=f2008 -fall-intrinsics -O2 -Dr16p"
  Linking flags: "-O2"
  Preprocessing flags: "-Dr16p"
  Coverage: False
  Profile: False
PreForM.py used: False
PreForM.py output directory: None
PreForM.py extensions processed: []

Building src/tests/basic_use.f90
Compiling src/lib/penf.F90 serially
Compiling src/lib/string_t.F90 serially
Compiling src/lib/stringifor.F90 serially
Compiling ...
Linking exe/basic_use
Target src/tests/basic_use.f90 has been successfully built
...

→ tree -L 1 exe/
exe/
├── basic_use
...
```

## Documentation

Besides this README file the FURY documentation is contained into its own [wiki](https://github.com/szaghi/FURY/wiki). Detailed documentation of the API is contained into the [GitHub Pages](http://szaghi.github.io/FURY/index.html) that can also be created locally by means of [ford tool](https://github.com/cmacmackin/ford).

### A Taste of FURY

To be written.

Go to [Top](#top)
