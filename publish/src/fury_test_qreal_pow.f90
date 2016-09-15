!< FURY test of [[qreal]].
program fury_test_qreal_pow
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[qreal]].
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(qreal64) :: force1         !< A force.
type(qreal64) :: force2         !< A force.
type(uom64)   :: u_acceleration !< Acceleration unit.
type(uom64)   :: u_force        !< Force unit.
type(uom64)   :: u_length       !< Length unit.
type(uom64)   :: u_mass         !< Mass unit.
type(uom64)   :: u_speed        !< Speed unit.
type(uom64)   :: u_time         !< Time unit.
logical       :: test_passed(7) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

u_length = uom64(source='m [length]', name='metre')
u_mass = uom64(source='kg [mass]', name='kilogram')
u_time = uom64(source='s [time]', name='second')

u_speed = u_length / u_time
u_acceleration = u_speed / u_time
u_force = u_mass * u_acceleration

force1 = qreal64(magnitude=2._R_P, unit=u_force)

force2 = force1 ** 2._R16P ; test_passed(1) = force2%stringify(format='(F3.1)')=='4.0 kg2.m2.s-4'
print "(A,L1)", '(2.0 kg.m.s-2) ** 2._R16P = '//force2%stringify(format='(F3.1)')//', is correct? ', test_passed(1)
force2 = force1 ** 2._R8P  ; test_passed(2) = force2%stringify(format='(F3.1)')=='4.0 kg2.m2.s-4'
print "(A,L1)", '(2.0 kg.m.s-2) ** 2._R8P  = '//force2%stringify(format='(F3.1)')//', is correct? ', test_passed(2)
force2 = force1 ** 2._R4P  ; test_passed(3) = force2%stringify(format='(F3.1)')=='4.0 kg2.m2.s-4'
print "(A,L1)", '(2.0 kg.m.s-2) ** 2._R4P  = '//force2%stringify(format='(F3.1)')//', is correct? ', test_passed(3)
force2 = force1 ** 2_I8P   ; test_passed(4) = force2%stringify(format='(F3.1)')=='4.0 kg2.m2.s-4'
print "(A,L1)", '(2.0 kg.m.s-2) ** 2_I8P   = '//force2%stringify(format='(F3.1)')//', is correct? ', test_passed(4)
force2 = force1 ** 2_I4P   ; test_passed(5) = force2%stringify(format='(F3.1)')=='4.0 kg2.m2.s-4'
print "(A,L1)", '(2.0 kg.m.s-2) ** 2_I4P   = '//force2%stringify(format='(F3.1)')//', is correct? ', test_passed(5)
force2 = force1 ** 2_I2P   ; test_passed(6) = force2%stringify(format='(F3.1)')=='4.0 kg2.m2.s-4'
print "(A,L1)", '(2.0 kg.m.s-2) ** 2_I2P   = '//force2%stringify(format='(F3.1)')//', is correct? ', test_passed(6)
force2 = force1 ** 2_I1P   ; test_passed(7) = force2%stringify(format='(F3.1)')=='4.0 kg2.m2.s-4'
print "(A,L1)", '(2.0 kg.m.s-2) ** 2_I1P   = '//force2%stringify(format='(F3.1)')//', is correct? ', test_passed(7)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_qreal_pow
