!< FURY qreal div test.
program div_qreal
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY qreal div test.
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
type(qreal)                   :: speed          !< The speed.
type(qreal)                   :: length(2)      !< The length.
type(qreal)                   :: time           !< The time.
type(units_system_si), target :: si             !< SI system.
logical                       :: test_passed(2) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

call si%initialize

length(1) = qreal(magnitude=1._R_P, unit=si%metre, units_system=si)
length(2) = qreal(magnitude=1._R_P, unit=si%metre, units_system=si)
time = qreal(magnitude=1._R_P, unit=si%second, units_system=si)

speed = (length(1) + length(2))/ time
test_passed(1) = speed%magnitude==2._R_P
print "(A,L1)", '(1m + 1m)/(1s) = '//speed%stringify(format='(F3.1)')//', is correct? ', test_passed(1)

speed = time / (length(1) + length(2))
test_passed(2) = speed%magnitude==0.5_R_P
print "(A,L1)", '(1s)/(1m + 1m) = '//speed%stringify(format='(F3.1)')//', is correct? ', test_passed(2)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram div_qreal
