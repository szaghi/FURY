!< FURY qreal mul test.
program mul_qreal
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY qreal mul test.
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
type(qreal)                   :: length(2)      !< The length.
type(qreal)                   :: area           !< The area.
type(qreal)                   :: speed          !< The speed.
type(units_system_si), target :: si             !< SI system.
logical                       :: test_passed(2) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

call si%initialize

length(1) = qreal(magnitude=2._R_P, unit=si%metre, units_system=si)
length(2) = qreal(magnitude=2._R_P, unit=si%metre, units_system=si)
area = length(1) * length(2)
test_passed(1) = area%magnitude==4._R_P
print "(A,L1)", '2m * 2m = '//area%stringify(format='(F3.1)')//', is correct? ', test_passed(1)

speed = qreal(magnitude=2._R_P, unit=si%metre_per_second, units_system=si)
area = speed * speed
test_passed(2) = area%magnitude==4._R_P
print "(A,L1)", '2m/s * 2m/s = '//area%stringify(format='(F3.1)')//', is correct? ', test_passed(2)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram mul_qreal
