!< FURY test of [[uom]].
program fury_test_uom_sub_failure
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[uom]].
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(uom) :: a_unit         !< A unit.
type(uom) :: si_length      !< SI length unit.
type(uom) :: si_speed       !< SI speed unit.
logical   :: test_passed(1) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.
print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)

print "(A)", 'An error will be raised (if all go rigth)'
si_length = uom(source='m')
si_speed = uom(source='m.s-1')
a_unit = si_length - si_speed ! lhs unit /= rhs, lhs-rhs has no sense

print "(A)", 'ERROR: the test should not reach this point, a previous error should have stop it before!'
print "(A,L1)", new_line('a')//'Are all tests passed? ', .true.
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_uom_sub_failure
