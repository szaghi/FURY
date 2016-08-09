!< FURY qreal add/sub incompatible test.
program add_sub_incompatible_qreal
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY qreal add/sub incompatible test.
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
type(qreal)      :: q              !< A quantity.
type(qreal)      :: q1             !< A quantity.
type(qreal)      :: q2             !< A quantity.
logical          :: test_passed(4) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

! initialize SI
call initialize_si

q1 = qreal(magnitude=1._R_P, unit=metre)
q2 = qreal(magnitude=1._R_P, unit=second)

q = q1 + q2
test_passed(1) = q%magnitude==0._R_P
print "(A,L1)", '1m + 1s = '//q%stringify(format='(F3.1)')//'<=>"incompatible!", is correct? ', test_passed(1)

q = q2 + q1
test_passed(2) = q%magnitude==0._R_P
print "(A,L1)", '1s + 1m = '//q%stringify(format='(F3.1)')//'<=>"incompatible!", is correct? ', test_passed(2)

q = q1 - q2
test_passed(3) = q%magnitude==0._R_P
print "(A,L1)", '1m - 1s = '//q%stringify(format='(F3.1)')//'<=>"incompatible!", is correct? ', test_passed(3)

q = q2 - q1
test_passed(4) = q%magnitude==0._R_P
print "(A,L1)", '1s - 1m = '//q%stringify(format='(F3.1)')//'<=>"incompatible!", is correct? ', test_passed(4)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram add_sub_incompatible_qreal
