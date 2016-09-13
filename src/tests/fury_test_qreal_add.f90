!< FURY test of [[qreal]].
program fury_test_qreal_add
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[qreal]].
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(qreal64) :: force1         !< A force.
type(qreal64) :: force2         !< A force.
type(qreal64) :: force3         !< A force.
type(qreal32) :: force4         !< A force.
type(uom64)   :: u_acceleration !< Acceleration unit.
type(uom64)   :: u_force        !< Force unit.
type(uom64)   :: u_length       !< Length unit.
type(uom64)   :: u_mass         !< Mass unit.
type(uom64)   :: u_speed        !< Speed unit.
type(uom64)   :: u_time         !< Time unit.
logical       :: test_passed(3) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

u_length = uom64(source='m [length]', name='metre')
print "(A)", 'u_length = '//u_length%stringify(with_dimensions=.true., with_name=.true.)
u_mass = uom64(source='kg [mass]', name='kilogram')
print "(A)", 'u_mass = '//u_mass%stringify(with_dimensions=.true., with_name=.true.)
u_time = uom64(source='s [time]', name='second')
print "(A)", 'u_time = '//u_time%stringify(with_dimensions=.true., with_name=.true.)

u_speed = u_length / u_time
print "(A)", 'u_speed = '//u_speed%stringify(with_dimensions=.true., with_name=.true.)
u_acceleration = u_speed / u_time
print "(A)", 'u_acceleration = '//u_acceleration%stringify(with_dimensions=.true., with_name=.true.)
u_force = u_mass * u_acceleration
print "(A)", 'u_force = '//u_force%stringify(with_dimensions=.true., with_name=.true.)

force1 = qreal64(magnitude=1._R8P, unit=u_force)
force2 = qreal64(magnitude=2._R8P, unit=u_force)
print "(A)", 'force 1 = '//force1%stringify(format='(F3.1)', with_dimensions=.true., with_name=.true.)
print "(A)", 'force 2 = '//force1%stringify(format='(F3.1)', with_dimensions=.true., with_name=.true.)

force3 = force1 + force2
test_passed(1) = force3%stringify(format='(F3.1)')=='3.0 kg.m.s-2'
print "(A,L1)", 'force1 + force2 = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(1)

force4 = force2
call force3%unset
force3 = force1 + force4
test_passed(2) = force3%stringify(format='(F3.1)')=='3.0 kg.m.s-2'
print "(A,L1)", 'force1(float64) + force2(float32) = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(2)

force3 = force4 + force1
test_passed(3) = force3%stringify(format='(F3.1)')=='3.0 kg.m.s-2'
print "(A,L1)", 'force1(float32) + force2(float64) = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(3)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_qreal_add
