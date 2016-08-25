!< FURY test of [[unit generic]].
program fury_test_qreal
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[qreal]].
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(qreal)        :: force1          !< A force.
type(qreal)        :: force2          !< A force.
type(qreal)        :: force3          !< A force.
type(unit_generic) :: u_acceleration  !< Acceleration unit.
type(unit_generic) :: u_force         !< Force unit.
type(unit_generic) :: u_length        !< Length unit.
type(unit_generic) :: u_mass          !< Mass unit.
type(unit_generic) :: u_speed         !< Speed unit.
type(unit_generic) :: u_time          !< Time unit.
logical            :: test_passed(27) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

u_length = unit_generic(source='m [length]', name='metre')
u_mass = unit_generic(source='kg [mass]', name='kilogram')
u_time = unit_generic(source='s [time]', name='second')

u_speed = u_length / u_time
u_acceleration = u_speed / u_time
u_force = u_mass * u_acceleration

force1 = qreal(magnitude=1._R_P, unit=u_force)
force2 = qreal(magnitude=2._R_P, unit=u_force)

force3 = force1
test_passed(1) = force3%stringify(format='(F3.1)')=='1.0 kg.m.s-2'
print "(A,L1)", '1.0 kg.m.s-2 = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(1)

force3 = force1 + force2
test_passed(2) = force3%stringify(format='(F3.1)')=='3.0 kg.m.s-2'
print "(A,L1)", '1.0 kg.m.s-2 + 2.0 kg.m.s-2 = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(2)

force3 = force1 - force2
test_passed(3) = force3%stringify(format='(F4.1)')=='-1.0 kg.m.s-2'
print "(A,L1)", '1.0 kg.m.s-2 - 2.0 kg.m.s-2 = '//force3%stringify(format='(F4.1)')//', is correct? ', test_passed(3)

call force3%unset
force3 = force1 / force2
test_passed(4) = force3%stringify(format='(F3.1)')=='0.5 kg0.m0.s0'
print "(A,L1)", '1.0 kg.m.s-2 / 2.0 kg.m.s-2 = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(4)

call force3%unset
force3 = force1 * force2
test_passed(5) = force3%stringify(format='(F3.1)')=='2.0 kg2.m2.s-4'
print "(A,L1)", '1.0 kg.m.s-2 * 2.0 kg.m.s-2 = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(5)

call force3%unset

force3 = force1 * 2._R8P
test_passed(6) = force3%stringify(format='(F3.1)')=='2.0 kg.m.s-2'
print "(A,L1)", '2._R8P * 1.0 kg.m.s-2 = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(6)

force3 = force1 * 2._R4P
test_passed(7) = force3%stringify(format='(F3.1)')=='2.0 kg.m.s-2'
print "(A,L1)", '2._R4P * 1.0 kg.m.s-2 = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(7)

force3 = force1 * 2_I8P
test_passed(8) = force3%stringify(format='(F3.1)')=='2.0 kg.m.s-2'
print "(A,L1)", '2_I8P * 1.0 kg.m.s-2 = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(8)

force3 = force1 * 2_I4P
test_passed(9) = force3%stringify(format='(F3.1)')=='2.0 kg.m.s-2'
print "(A,L1)", '2_I4P * 1.0 kg.m.s-2 = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(9)

force3 = force1 * 2_I2P
test_passed(10) = force3%stringify(format='(F3.1)')=='2.0 kg.m.s-2'
print "(A,L1)", '2_I2P * 1.0 kg.m.s-2 = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(10)

force3 = force1 * 2_I1P
test_passed(11) = force3%stringify(format='(F3.1)')=='2.0 kg.m.s-2'
print "(A,L1)", '2_I1P * 1.0 kg.m.s-2 = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(11)

force3 = 2._R8P *  force1
test_passed(12) = force3%stringify(format='(F3.1)')=='2.0 kg.m.s-2'
print "(A,L1)", '1.0 kg.m.s-2 * 2._R8P = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(12)

force3 = 2._R4P *  force1
test_passed(13) = force3%stringify(format='(F3.1)')=='2.0 kg.m.s-2'
print "(A,L1)", '1.0 kg.m.s-2 * 2._R4P = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(13)

force3 = 2_I8P * force1
test_passed(14) = force3%stringify(format='(F3.1)')=='2.0 kg.m.s-2'
print "(A,L1)", '1.0 kg.m.s-2 * 2_I8P = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(14)

force3 = 2_I4P * force1
test_passed(15) = force3%stringify(format='(F3.1)')=='2.0 kg.m.s-2'
print "(A,L1)", '1.0 kg.m.s-2 * 2_I4P = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(15)

force3 = 2_I2P * force1
test_passed(16) = force3%stringify(format='(F3.1)')=='2.0 kg.m.s-2'
print "(A,L1)", '1.0 kg.m.s-2 * 2_I2P = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(16)

force3 = 2_I1P * force1
test_passed(17) = force3%stringify(format='(F3.1)')=='2.0 kg.m.s-2'
print "(A,L1)", '1.0 kg.m.s-2 * 2_I1P = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(17)

force3 = force1 / 2._R8P
test_passed(18) = force3%stringify(format='(F3.1)')=='0.5 kg.m.s-2'
print "(A,L1)", '2._R8P / 1.0 kg.m.s-2 = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(18)

force3 = force1 / 2._R4P
test_passed(19) = force3%stringify(format='(F3.1)')=='0.5 kg.m.s-2'
print "(A,L1)", '2._R4P / 1.0 kg.m.s-2 = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(19)

force3 = force1 / 2_I8P
test_passed(20) = force3%stringify(format='(F3.1)')=='0.5 kg.m.s-2'
print "(A,L1)", '2_I8P / 1.0 kg.m.s-2 = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(20)

force3 = force1 / 2_I4P
test_passed(21) = force3%stringify(format='(F3.1)')=='0.5 kg.m.s-2'
print "(A,L1)", '2_I4P / 1.0 kg.m.s-2 = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(21)

force3 = force1 / 2_I2P
test_passed(22) = force3%stringify(format='(F3.1)')=='0.5 kg.m.s-2'
print "(A,L1)", '2_I2P / 1.0 kg.m.s-2 = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(22)

force3 = force1 / 2_I1P
test_passed(23) = force3%stringify(format='(F3.1)')=='0.5 kg.m.s-2'
print "(A,L1)", '2_I1P / 1.0 kg.m.s-2 = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(23)

call force3%unset
force3 = force2 ** 2_I8P
test_passed(24) = force3%stringify(format='(F3.1)')=='4.0 kg2.m2.s-4'
print "(A,L1)", '(2.0 kg.m.s-2) ** 2_I8P = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(24)

call force3%unset
force3 = force2 ** 2_I4P
test_passed(25) = force3%stringify(format='(F3.1)')=='4.0 kg2.m2.s-4'
print "(A,L1)", '(2.0 kg.m.s-2) ** 2_I4P = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(25)

call force3%unset
force3 = force2 ** 2_I2P
test_passed(26) = force3%stringify(format='(F3.1)')=='4.0 kg2.m2.s-4'
print "(A,L1)", '(2.0 kg.m.s-2) ** 2_I2P = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(26)

call force3%unset
force3 = force2 ** 2_I1P
test_passed(27) = force3%stringify(format='(F3.1)')=='4.0 kg2.m2.s-4'
print "(A,L1)", '(2.0 kg.m.s-2) ** 2_I1P = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(27)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_qreal
