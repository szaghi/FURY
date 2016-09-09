!< FURY test of [[uom]].
program fury_test_uom_assign_failure
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[uom]].
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(uom64) :: si_length      !< SI length unit.
type(uom64) :: si_speed       !< SI speed unit.
logical     :: test_passed(1) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.
print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)

print "(A)", 'An error will be raised (if all go rigth)'
si_length = uom64(source='m')
si_speed = uom64(source='m.s-1')
si_length = si_speed ! lhs has already a unit /= rhs

print "(A)", 'ERROR: the test should not reach this point, a previous error should have stop it before!'
print "(A,L1)", new_line('a')//'Are all tests passed? ', .true.
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_uom_assign_failure
