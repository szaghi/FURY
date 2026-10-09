program cookbook_impulse
!< Cookbook: convert a compound quantity, pound-force seconds into newton seconds.
use fury
implicit none
type(uom64)   :: newton, pound_force, second
type(qreal64) :: impulse, force, impulse_si

newton      = uom64('kg [mass].m [length].s-2 [time-2] (N[force]) {newton}')
pound_force = uom64('lbf< = 4.4482216152605 * N> [force] {pound_force}')
second      = uom64('s [time] {second}')
impulse    = 10._R8P * (pound_force * second)
force      = impulse / (1._R8P * second)     ! back to a force, lbf
impulse_si = force%to(newton) * (1._R8P * second)
print '(A)', impulse%stringify(format='(F4.1)')//' = '//impulse_si%stringify(format='(F6.3)')
endprogram cookbook_impulse
