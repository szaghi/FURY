!< FURY test of [[uom]].
program fury_test_uom_pow
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[uom]].
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(uom64) :: si_mass        !< SI mass unit.
type(uom64) :: a_unit         !< A unit.
logical     :: test_passed(7) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

si_mass = uom64(source='kg [mass]', name='kilogram')

a_unit = si_mass ** 2._R16P ; test_passed(1) = a_unit%stringify()=='kg2'
print "(A,L1)", 'si_mass ** 2._R16P = '//a_unit%stringify(with_dimensions=.true.)//', is correct? ', test_passed(1)
a_unit = si_mass ** 2._R8P  ; test_passed(2) = a_unit%stringify()=='kg2'
print "(A,L1)", 'si_mass ** 2._R8P  = '//a_unit%stringify(with_dimensions=.true.)//', is correct? ', test_passed(2)
a_unit = si_mass ** 2._R4P  ; test_passed(3) = a_unit%stringify()=='kg2'
print "(A,L1)", 'si_mass ** 2._R4P  = '//a_unit%stringify(with_dimensions=.true.)//', is correct? ', test_passed(3)
a_unit = si_mass ** 2_I8P   ; test_passed(4) = a_unit%stringify()=='kg2'
print "(A,L1)", 'si_mass ** 2_I8P   = '//a_unit%stringify(with_dimensions=.true.)//', is correct? ', test_passed(4)
a_unit = si_mass ** 2_I4P   ; test_passed(5) = a_unit%stringify()=='kg2'
print "(A,L1)", 'si_mass ** 2_I4P   = '//a_unit%stringify(with_dimensions=.true.)//', is correct? ', test_passed(5)
a_unit = si_mass ** 2_I2P   ; test_passed(6) = a_unit%stringify()=='kg2'
print "(A,L1)", 'si_mass ** 2_I2P   = '//a_unit%stringify(with_dimensions=.true.)//', is correct? ', test_passed(6)
a_unit = si_mass ** 2_I1P   ; test_passed(7) = a_unit%stringify()=='kg2'
print "(A,L1)", 'si_mass ** 2_I1P   = '//a_unit%stringify(with_dimensions=.true.)//', is correct? ', test_passed(7)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_uom_pow
