!< FURY test of [[unit_generic]].
program fury_test_unit_generic_sub_failure
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[unit_generic]].
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(unit_generic) :: a_unit         !< A unit.
type(unit_generic) :: si_length      !< SI length unit.
type(unit_generic) :: si_speed       !< SI speed unit.
logical            :: test_passed(1) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.
print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)

print "(A)", 'An error will be raised (if all go rigth)'
si_length = unit_generic(source='m')
si_speed = unit_generic(source='m.s-1')
a_unit = si_length - si_speed ! lhs unit /= rhs, lhs-rhs has no sense

print "(A)", 'ERROR: the test should not reach this point, a previous error should have stop it before!'
print "(A,L1)", new_line('a')//'Are all tests passed? ', .true.
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_unit_generic_sub_failure
