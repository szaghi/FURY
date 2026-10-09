---
layout: home

hero:
  name: FURY
  text: Fortran Units (environment) for Reliable phYsical math
  tagline: "Numbers that know their units. FURY checks every sum, assignment and conversion of your physical quantities, derives the units of products and powers, and stops the program at the first inconsistency, instead of letting a wrong number through."
  actions:
    - theme: brand
      text: Tutorial
      link: /manual/tutorial/01-first-quantity
    - theme: alt
      text: Cookbook
      link: /manual/cookbook
    - theme: alt
      text: Reference
      link: /guide/features
    - theme: alt
      text: API
      link: /api/
    - theme: alt
      text: View on GitHub
      link: https://github.com/szaghi/FURY

features:
  - icon: 📏
    title: Quantities with units
    details: "A magnitude and its unit of measure in one object: qreal64 = 9.58_R8P * second. Arithmetic, powers, comparisons: the units follow the numbers."
    link: /manual/tutorial/01-first-quantity
    linkText: A first quantity
  - icon: 🛑
    title: Inconsistencies stop the program
    details: "Metres plus seconds, a length assigned to a time, a conversion between unrelated units: FURY prints what is wrong and stops, with a non-zero exit status."
    link: /manual/tutorial/04-consistency
    linkText: What FURY refuses
  - icon: 🧮
    title: Symbolic algebra on units
    details: "Pa times m2 is kg.m.s-2; m.s-1 times s is m. Products, quotients and powers of units are derived symbolically, dimensions included."
    link: /manual/tutorial/03-unit-algebra
    linkText: The algebra of units
  - icon: ✍️
    title: Units from a string
    details: "'kg [mass].m [length].s-2 [time-2] (N[force]) {newton}': symbols, aliases, conversion factors and offsets, dimensions, a main alias and a name, in one readable definition."
    link: /guide/grammar
    linkText: Unit grammar
  - icon: 🔁
    title: Conversions
    details: "Factors (km = 1000 * m), offsets (degC = 273.15 + K), conversions through a common unit (ft to km through m), and your own non-linear formulas (dBm to mW)."
    link: /manual/tutorial/05-conversions
    linkText: Conversions
  - icon: 🌍
    title: The SI system, ready to use
    details: "Base and derived units, prefixes from yocto to yotta and kibi to yobi, physical constants: queried by name, symbol or synonym, prefixed or not (km, kilometre)."
    link: /manual/tutorial/06-si-system
    linkText: The SI system
  - icon: 🎯
    title: Three precisions
    details: "Every type in 32, 64 and 128 bits, with mixed-kind arithmetic; the 128 bits kinds are optional, for compilers without quadruple precision."
    link: /manual/tutorial/07-precision
    linkText: Precision
  - icon: 🛠️
    title: Standard Fortran
    details: "Pure Fortran 2018, OOP designed, tested with gfortran 14 and 16; built with FoBiS or fpm. Free and open source, under GPL, BSD or MIT."
    link: /guide/install
    linkText: Installation
---
