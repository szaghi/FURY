!< FURY test of [[qreal]].
program fury_test_qreal_add
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[qreal]].
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(qreal) :: force1         !< A force.
type(qreal) :: force2         !< A force.
type(qreal) :: force3         !< A force.
type(uom)   :: u_acceleration !< Acceleration unit.
type(uom)   :: u_force        !< Force unit.
type(uom)   :: u_length       !< Length unit.
type(uom)   :: u_mass         !< Mass unit.
type(uom)   :: u_speed        !< Speed unit.
type(uom)   :: u_time         !< Time unit.
logical     :: test_passed(1) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

u_length = uom(source='m [length]', name='metre')
print "(A)", 'u_length = '//u_length%stringify(with_dimensions=.true., with_name=.true.)
u_mass = uom(source='kg [mass]', name='kilogram')
print "(A)", 'u_mass = '//u_mass%stringify(with_dimensions=.true., with_name=.true.)
u_time = uom(source='s [time]', name='second')
print "(A)", 'u_time = '//u_time%stringify(with_dimensions=.true., with_name=.true.)

u_speed = u_length / u_time
print "(A)", 'u_speed = '//u_speed%stringify(with_dimensions=.true., with_name=.true.)
u_acceleration = u_speed / u_time
print "(A)", 'u_acceleration = '//u_acceleration%stringify(with_dimensions=.true., with_name=.true.)
u_force = u_mass * u_acceleration
print "(A)", 'u_force = '//u_force%stringify(with_dimensions=.true., with_name=.true.)

force1 = qreal(magnitude=1._R_P, unit=u_force)
force2 = qreal(magnitude=2._R_P, unit=u_force)
print "(A)", 'force 1 = '//force1%stringify(format='(F3.1)', with_dimensions=.true., with_name=.true.)
print "(A)", 'force 2 = '//force1%stringify(format='(F3.1)', with_dimensions=.true., with_name=.true.)

force3 = force1 + force2
test_passed(1) = force3%stringify(format='(F3.1)')=='3.0 kg.m.s-2'
print "(A,L1)", 'force1 + force2 = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(1)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_qreal_add
