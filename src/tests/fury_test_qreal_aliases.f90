!< FURY test of [[qreal]].
program fury_test_qreal_aliases
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
logical     :: test_passed(2) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

u_speed_km_h = 'km = 1000 * m [length].h-1 = 3600 * s-1 [time-1](km/h[speed]){km/h}'
u_speed_m_s = 'm [length].s-1 [time-1](m/s[speed]){m/s}'

speed1 = qreal(magnitude=36._R_P, unit=u_speed_km_h)
speed2 = qreal(magnitude=2._R_P, unit=u_speed_m_s)

speed3 = speed1%to(u_speed_m_s) + speed2
test_passed(1) = speed3%stringify(format='(F7.5)')=='2.00001 m.s-1'
print "(A,L1)", '36.0 km.h-1 + 2.0 m.s = '//speed3%stringify(format='(F7.5)')//', is correct? ', test_passed(1)

call speed3%unset
speed3 = speed1 + speed2%to(u_speed_km_h)
test_passed(2) = speed3%stringify(format='(F4.1)')=='38.0 km.h-1'
print "(A,L1)", '36.0 km.h-1 + 2.0 m.s = '//speed3%stringify(format='(F4.1)')//', is correct? ', test_passed(2)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_qreal_aliases
