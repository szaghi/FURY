!< FURY test of [[qreal]].
program fury_test_qreal_mul_mixed
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[qreal]].
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(qreal32)  :: force1         !< A force.
type(qreal64)  :: force2         !< A force.
type(qreal64)  :: force3         !< A force.
type(qreal128) :: force4         !< A force.
type(uom64)    :: u_acceleration !< Acceleration unit.
type(uom64)    :: u_force        !< Force unit.
type(uom64)    :: u_length       !< Length unit.
type(uom64)    :: u_mass         !< Mass unit.
type(uom64)    :: u_speed        !< Speed unit.
type(uom64)    :: u_time         !< Time unit.
logical        :: test_passed(6) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

u_length = uom64(source='m [length]', name='metre')
u_mass = uom64(source='kg [mass]', name='kilogram')
u_time = uom64(source='s [time]', name='second')

u_speed = u_length / u_time
u_acceleration = u_speed / u_time
u_force = u_mass * u_acceleration

force1 = 1 * u_force
force2 = 2 * u_force
force4 = force1

force3 = force1 * force2
test_passed(1) = force3%stringify(format='(F3.1)')=='2.0 kg2.m2.s-4'
print "(A,L1)", 'force(float32) * force(float64) = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(1)

force3 = force2 * force1
test_passed(2) = force3%stringify(format='(F3.1)')=='2.0 kg2.m2.s-4'
print "(A,L1)", 'force(float64) * force(float32) = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(1)

call force3%unset
force3 = force1 * force4
test_passed(3) = force3%stringify(format='(F3.1)')=='1.0 kg2.m2.s-4'
print "(A,L1)", 'force(float32) * force(float128) = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(3)

call force3%unset
force3 = force4 * force1
test_passed(4) = force3%stringify(format='(F3.1)')=='1.0 kg2.m2.s-4'
print "(A,L1)", 'force(float128) * force(float32) = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(4)

call force3%unset
force3 = force2 * force4
test_passed(5) = force3%stringify(format='(F3.1)')=='2.0 kg2.m2.s-4'
print "(A,L1)", 'force(float64) * force(float128) = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(5)

call force3%unset
force3 = force4 * force2
test_passed(6) = force3%stringify(format='(F3.1)')=='2.0 kg2.m2.s-4'
print "(A,L1)", 'force(float128) * force(float64) = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(6)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_qreal_mul_mixed
