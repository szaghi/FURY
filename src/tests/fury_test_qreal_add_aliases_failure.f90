!< FURY test of [[qreal]].
program fury_test_qreal_add_aliases_failure
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[qreal]].
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(qreal) :: speed1         !< A speed.
type(qreal) :: speed2         !< A speed.
type(qreal) :: speed3         !< A speed.
type(uom)   :: u_speed_km_h   !< Speed unit.
type(uom)   :: u_speed_m_s    !< Speed unit.
logical     :: test_passed(1) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.
print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)

u_speed_km_h = uom('km = 1000 * m [length].h-1 = 3600 * s-1 [time-1](km/h[speed]){km/h}')
u_speed_m_s = uom('m [length].s-1 [time-1](m/s[speed]){m/s}')

speed1 = qreal(magnitude=1._R_P, unit=u_speed_km_h)
speed2 = qreal(magnitude=2._R_P, unit=u_speed_m_s)

print "(A)", 'An error will be raised (if all go rigth)'
speed3 = speed1 + speed2
test_passed(1) = speed3%stringify(format='(F3.1)')=='3.0 km.h-1'
print "(A,L1)", '1.0 km.h-1 + 2.0 m.s = '//speed3%stringify(format='(F3.1)')//', is correct? ', test_passed(1)

print "(A)", 'ERROR: the test should not reach this point, a previous error should have stop it before!'
print "(A,L1)", new_line('a')//'Are all tests passed? ', .true.
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_qreal_add_aliases_failure
