program tutorial_1
!< Tutorial 1: a first quantity, a magnitude with its unit of measure.
use fury
implicit none
type(uom64)   :: metre, second
type(qreal64) :: distance, time, speed, distance_again

!region define
metre  = uom64('m [length]')
second = uom64('s [time]')
!endregion define

!region quantities
distance = qreal64(magnitude=100._R8P, unit=metre)  ! the constructor
time     = 9.58_R8P * second                         ! a number times a unit
!endregion quantities

!region compute
speed = distance / time
distance_again = speed * time
!endregion compute

!region print
print '(A)', speed%stringify()
print '(A)', speed%stringify(format='(F6.3)')
print '(A)', speed%stringify(compact_reals=.true., with_dimensions=.true.)
print '(A)', distance_again%stringify(format='(F6.2)')
print '(F6.3)', speed%magnitude
!endregion print
!run tutorial_1 tutorial_1
endprogram tutorial_1
