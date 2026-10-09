---
layout: home

hero:
  name: FURY
  text: Fortran Units (environment) for Reliable phYsical math
  tagline: "Physical quantities that carry their unit of measure: dimensions checked, scaling factors propagated, inconsistent operations stopped. Pure Fortran, OOP designed."
  actions:
    - theme: brand
      text: Quick Start
      link: /guide/quickstart
    - theme: alt
      text: Installation
      link: /guide/installation
    - theme: alt
      text: View on GitHub
      link: https://github.com/szaghi/FURY

features:
  - title: Quantities with units
    details: "qreal32, qreal64, qreal128: a magnitude plus its unit of measure; +, -, *, /, ** and comparisons check and propagate the units."
    link: /guide/quickstart
    linkText: Quick Start
  - title: Symbolic algebra on units
    details: "uom32, uom64, uom128: units parsed from strings such as 'm = meter = metre [length] {meter}', multiplied, divided and raised to powers symbolically."
    link: /guide/
    linkText: About
  - title: SI system
    details: "system_si32, system_si64, system_si128: the SI base and derived units, prefixes and physical constants, queried by name."
    link: /guide/
    linkText: About
---
