!< FURY test of [[qreal]].
program fury_test_qreal_pow
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
logical     :: test_passed(4) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

u_length = uom(source='m [length]', name='metre')
u_mass = uom(source='kg [mass]', name='kilogram')
u_time = uom(source='s [time]', name='second')

u_speed = u_length / u_time
u_acceleration = u_speed / u_time
u_force = u_mass * u_acceleration

force1 = qreal(magnitude=1._R_P, unit=u_force)
force2 = qreal(magnitude=2._R_P, unit=u_force)

call force3%unset
force3 = force2 ** 2_I8P
test_passed(1) = force3%stringify(format='(F3.1)')=='4.0 kg2.m2.s-4'
print "(A,L1)", '(2.0 kg.m.s-2) ** 2_I8P = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(1)

call force3%unset
force3 = force2 ** 2_I4P
test_passed(2) = force3%stringify(format='(F3.1)')=='4.0 kg2.m2.s-4'
print "(A,L1)", '(2.0 kg.m.s-2) ** 2_I4P = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(2)

call force3%unset
force3 = force2 ** 2_I2P
test_passed(3) = force3%stringify(format='(F3.1)')=='4.0 kg2.m2.s-4'
print "(A,L1)", '(2.0 kg.m.s-2) ** 2_I2P = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(3)

call force3%unset
force3 = force2 ** 2_I1P
test_passed(4) = force3%stringify(format='(F3.1)')=='4.0 kg2.m2.s-4'
print "(A,L1)", '(2.0 kg.m.s-2) ** 2_I1P = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(4)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_qreal_pow
