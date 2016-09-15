!< FURY test of [[qreal]].
program fury_test_qreal_add_failure
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[qreal]].
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(qreal64) :: speed          !< A speed.
type(qreal64) :: time           !< A time.
type(qreal64) :: to_fail        !< Quantity faliling.
type(uom64)   :: u_speed        !< Speed unit.
type(uom64)   :: u_time         !< Time unit.
logical       :: test_passed(1) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.
print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)

u_speed = uom64('km = 1000 * m [length].h-1 = 3600 s-1 [time-1](km/h[speed]){km/h}')
u_time = uom64('s = second [time]')

speed = 2 * u_speed
time = 2 * u_time

print "(A)", 'An error will be raised (if all go rigth)'
to_fail = speed + time
test_passed(1) = speed%stringify(format='(F3.1)')=='2.0 km.h-1'
print "(A,L1)", '2.0 km.h-1 + 2.0 s = '//speed%stringify(format='(F3.1)')//', is correct? ', test_passed(1)

print "(A)", 'ERROR: the test should not reach this point, a previous error should have stop it before!'
print "(A,L1)", new_line('a')//'Are all tests passed? ', .true.
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_qreal_add_failure
