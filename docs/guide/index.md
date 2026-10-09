# About

FURY is a pure Fortran library that gives physical computations a units-consistency check. A **quantity** (`qreal`)
is a magnitude together with its **unit of measure** (`uom`): every operation checks that the units are consistent,
propagates dimensions and multiplicative scaling factors, and stops the program with an error if the units are not
consistent (adding metres to metres per second, for example).

The project started from a [comp.lang.fortran discussion](https://groups.google.com/forum/#!topic/comp.lang.fortran/1TbfQlAmKx8)
started by W. Van Snyder, to whom FURY is dedicated: is a units-consistency-check facility desirable for Fortran, and can
it be implemented in Fortran?

## Main types

Every type comes in three real kinds, suffixed `32`, `64` and `128`:

| Type | Purpose |
|------|---------|
| `uom32/64/128` | unit of measure, parsed from a string definition; symbolic `*`, `/`, `**` |
| `qreal32/64/128` | physical quantity: real magnitude plus unit of measure |
| `system_si32/64/128` | the SI system: units, prefixes and constants queried by name |
| `uom_converter` | abstract converter for user-supplied, non-multiplicative conversions (e.g. dBm) |

Operators between quantities of different kinds are provided too (`qreal64 + qreal128`, ...).

## Unit definition grammar

A unit is defined by a string:

```
symbol = alias = alias [dimensions] {name}
```

for example `'m = meter = metre [length] {meter}'`. Aliases may carry a multiplicative factor or an additive
offset (`'km = 1000.0 * m'`, `'degC = 273.15 + K'`); complex units are products of symbols with integer exponents,
`'m.s-1'` for metres per second.

## License

FURY is free software, released under GPLv3, BSD-2, BSD-3 or MIT at your choice.
