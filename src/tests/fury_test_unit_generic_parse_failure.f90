!< FURY test of [[unit generic]].
program fury_test_unit_generic_parse_failure
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[unit generic]].
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(unit_generic) :: si_speed       !< SI speed unit.
logical            :: test_passed(1) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.
print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)

print "(A)", 'An error will be raised (if all go rigth)'
si_speed = unit_generic(symbols='m [length].s-1 [time2]') ! incosistent explicit dimension exponent

print "(A)", 'ERROR: the test should not reach this point, a previous error should have stop it before!'
print "(A,L1)", new_line('a')//'Are all tests passed? ', .true.
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_unit_generic_parse_failure
