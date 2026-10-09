program mco
!< The Mars Climate Orbiter mishap: an impulse in pound-force seconds summed to one in newton seconds.
use fury
implicit none
type(uom64)   :: newton, pound_force, second
type(qreal64) :: impulse_ground, impulse_spacecraft, total

newton      = uom64('kg [mass].m [length].s-2 [time-2] (N[force]) {newton}')
pound_force = uom64('lbf< = 4.4482216152605 * N> [force] {pound_force}')
second      = uom64('s [time] {second}')

impulse_spacecraft = 10._R8P * (newton * second)       ! the spacecraft expected newton seconds
impulse_ground     = 10._R8P * (pound_force * second)  ! the ground software produced pound-force seconds

print '(A)', 'spacecraft: '//impulse_spacecraft%stringify(format='(F5.1)', with_dimensions=.true.)
print '(A)', 'ground    : '//impulse_ground%stringify(format='(F5.1)', with_dimensions=.true.)
total = impulse_spacecraft + impulse_ground
print '(A)', 'total     : '//total%stringify(format='(F5.1)')
!run -s mco mco
!image mco
endprogram mco
