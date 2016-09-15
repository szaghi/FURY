!< FURY test of [[qreal]].
program fury_test_qreal_assign
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[qreal]].
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(qreal64)  :: force1         !< A force.
type(qreal64)  :: force2         !< A force.
type(qreal64)  :: force3         !< A force.
type(qreal32)  :: force4         !< A force.
type(qreal128) :: force5         !< A force.
type(uom64)    :: u_acceleration !< Acceleration unit.
type(uom64)    :: u_force        !< Force unit.
type(uom64)    :: u_length       !< Length unit.
type(uom64)    :: u_mass         !< Mass unit.
type(uom64)    :: u_speed        !< Speed unit.
type(uom64)    :: u_time         !< Time unit.
logical        :: test_passed(9) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

u_length = uom64(source='m [length]', name='metre')
u_mass = uom64(source='kg [mass]', name='kilogram')
u_time = uom64(source='s [time]', name='second')

u_speed = u_length / u_time
u_acceleration = u_speed / u_time
u_force = u_mass * u_acceleration

force1 = qreal64(magnitude=1._R_P, unit=u_force)
test_passed(1) = force1%stringify(format='(F3.1)')=='1.0 kg.m.s-2'
print "(A,L1)", '1.0 kg.m.s-2 = '//force1%stringify(format='(F3.1)')//', is correct? ', test_passed(1)

force2 = 2._R8P * u_force
test_passed(2) = force2%stringify(format='(F3.1)')=='2.0 kg.m.s-2'
print "(A,L1)", '1.0 kg.m.s-2 = '//force2%stringify(format='(F3.1)')//', is correct? ', test_passed(2)

force3 = force1
test_passed(3) = force3%stringify(format='(F3.1)')=='1.0 kg.m.s-2'
print "(A,L1)", '1.0 kg.m.s-2 = '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(3)

force4 = force1
test_passed(4) = force4%stringify(format='(F3.1)')=='1.0 kg.m.s-2'
print "(A,L1)", 'qreal32 = qreal64 => '//force4%stringify(format='(F3.1)')//', is correct? ', test_passed(4)

call force3%unset
force3 = force4
test_passed(5) = force3%stringify(format='(F3.1)')=='1.0 kg.m.s-2'
print "(A,L1)", 'qreal64 = qreal32 => '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(5)

force5 = force1
test_passed(6) = force5%stringify(format='(F3.1)')=='1.0 kg.m.s-2'
print "(A,L1)", 'qreal128 = qreal64 => '//force4%stringify(format='(F3.1)')//', is correct? ', test_passed(6)

call force3%unset
force3 = force5
test_passed(7) = force3%stringify(format='(F3.1)')=='1.0 kg.m.s-2'
print "(A,L1)", 'qreal64 = qreal128 => '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(7)

call force4%unset
force4 = force5
test_passed(8) = force5%stringify(format='(F3.1)')=='1.0 kg.m.s-2'
print "(A,L1)", 'qreal32 = qreal128 => '//force4%stringify(format='(F3.1)')//', is correct? ', test_passed(8)

call force5%unset
force5 = force3
test_passed(9) = force3%stringify(format='(F3.1)')=='1.0 kg.m.s-2'
print "(A,L1)", 'qreal128 = qreal32 => '//force3%stringify(format='(F3.1)')//', is correct? ', test_passed(9)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_qreal_assign
