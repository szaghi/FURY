# Quick Start

How fast is Bolt? Define the units, attach them to the magnitudes and divide:

```fortran
use, intrinsic :: iso_fortran_env, only : real64
use fury

type(uom64)   :: meter
type(uom64)   :: second
type(qreal64) :: distance_to_arrival
type(qreal64) :: time_to_arrival
type(qreal64) :: mean_velocity

meter = uom64('m = meter = metre [length] {meter}')
second = uom64('s = sec = second [time] {second}')

distance_to_arrival = qreal64(100._real64, meter)
time_to_arrival = qreal64(9.58_real64, second)

mean_velocity = distance_to_arrival / time_to_arrival

print "(A)", 'Bolt''s record speed: '//mean_velocity%stringify(with_dimensions=.true.)
```

prints

```
Bolt's record speed: +0.10438413361169102E+002 m.s-1 [length.time-1]
```

The quotient has the unit `m.s-1` and the dimensions `length.time-1`, both derived symbolically. Adding
`distance_to_arrival + mean_velocity` instead stops the program with an error (`error stop`): metres and metres per
second cannot be added.

## Steps

1. `use fury`;
2. declare the quantities, `type(qreal64) :: q`, and the units, `type(uom64) :: u`;
3. define the units from strings, `u = uom64('km = 1000.0 * m')`, or take them from the SI system:
   ```fortran
   type(system_si64) :: SI
   call SI%initialize
   u = SI%unit('metre')
   ```
4. attach magnitude and unit, `q = qreal64(1._real64, u)`;
5. compute: units are checked and propagated by every operator.
