# 7. Precision

Every FURY type comes in three real kinds, named by their bits:

| Kind | Units | Quantities | Systems | Real kind |
|---|---|---|---|---|
| 32 bits | `uom32` | `qreal32` | `system_si32` | `R4P` |
| 64 bits | `uom64` | `qreal64` | `system_si64` | `R8P` |
| 128 bits | `uom128` | `qreal128` | `system_si128` | `R16P` |

<<< @/examples/snippets/tutorial_7-kinds.f90

Quantities of different kinds can be summed, subtracted, multiplied, divided and compared; as for the intrinsic reals,
the result has the kind of the more precise operand. A quantity is assigned to one of another kind, with the conversion of
its magnitude:

<<< @/examples/snippets/tutorial_7-mixed.f90

<<< @/examples/output/tutorial_7.ansi{ansi}

The sum is computed in 128 bits, but its 64 bits operand carries only 16 significant digits: mixing kinds does not add
precision.

## Without quadruple precision

The 128 bits kinds need a compiler with quadruple precision reals, and FURY and PENF compiled with `-DPENF_R16P`. Without
the macro the 128 bits types do not exist, rather than being double precision in disguise: a program that uses
`qreal128` does not compile. The [Precision](/guide/precision) page has the details.

::: tip What you learned
The 32, 64 and 128 bits types, mixed-kind arithmetic and assignment, the optional quadruple precision.
Reference: [Precision](/guide/precision).
:::

Next: [8. Non-linear conversions](./08-converters).
