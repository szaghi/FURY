!< FURY qreal add/sub test.
program add_sub_qreal
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY qreal add/sub test.
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
type(qreal)      :: q              !< A quantity.
type(qreal)      :: q1             !< A quantity.
type(qreal)      :: q2             !< A quantity.
type(unit_metre) :: kilometre      !< The kilometre unit instance based on the metre unit.
logical          :: test_passed(4) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

! initialize SI
call initialize_si

! define a new unit, i.e. the kilometre that must be compatible with SI's metre
kilometre = unit_metre(scale_factor=1000._R_P, symbol='km') ! update also the metre symbols defining km a compatible unit

q1 = qreal(magnitude=1._R_P, unit=metre)
q2 = qreal(magnitude=1._R_P, unit=kilometre)
q = q1 + q2
test_passed(1) = q%magnitude==1001._R_P
print "(A,L1)", '1m + 1km = '//q%stringify(format='(F6.1)')//', is correct? ', test_passed(1)

q1 = qreal(magnitude=1._R_P, unit=kilometre)
q2 = qreal(magnitude=1._R_P, unit=metre)
q = q1 + q2
test_passed(2) = q%magnitude==1.001_R_P
print "(A,L1)", '1km + 1m = '//q%stringify(format='(F5.3)')//', is correct? ', test_passed(2)

q1 = qreal(magnitude=1._R_P, unit=metre)
q2 = qreal(magnitude=1._R_P, unit=kilometre)
q = q1 - q2
test_passed(3) = q%magnitude==-999._R_P
print "(A,L1)", '1m - 1km = '//q%stringify(format='(F6.1)')//', is correct? ', test_passed(3)

q1 = qreal(magnitude=1._R_P, unit=kilometre)
q2 = qreal(magnitude=1._R_P, unit=metre)
q = q1 - q2
test_passed(4) = q%magnitude==0.999_R_P
print "(A,L1)", '1km - 1m = '//q%stringify(format='(F5.3)')//', is correct? ', test_passed(4)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram add_sub_qreal

