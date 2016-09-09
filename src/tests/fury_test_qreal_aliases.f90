!< FURY test of [[qreal]].
program fury_test_qreal_aliases
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[qreal]].
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(qreal64) :: speed1         !< A speed.
type(qreal64) :: speed2         !< A speed.
type(qreal64) :: speed3         !< A speed.
type(uom64)   :: m              !< Meter unit.
type(uom64)   :: s              !< Second unit.
type(uom64)   :: km             !< Kilometer unit.
type(uom64)   :: h              !< Hour unit.
type(uom64)   :: km_h           !< Km/h unit.
type(uom64)   :: m_s            !< m/s unit.
logical       :: test_passed(2) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

m = uom64('m')
s = uom64('s')
h = uom64('h = 3600 * s')
km = uom64('km = 1000 * m')
km_h = km/h
m_s =  m/s

speed1 = 36 * km_h
speed2 = 2 * m_s

speed3 = speed1%to(m_s) + speed2
test_passed(1) = speed3%stringify(format='(F4.1)')=='12.0 m.s-1'
print "(A,L1)", '36.0 km.h-1 + 2.0 m.s-1 = '//speed3%stringify(format='(F4.1)')//', is correct? ', test_passed(1)

call speed3%unset
speed3 = speed1 + speed2%to(km_h)
test_passed(2) = speed3%stringify(format='(F4.1)')=='43.2 km.h-1'
print "(A,L1)", '36.0 km.h-1 + 2.0 m.s-1 = '//speed3%stringify(format='(F4.1)')//', is correct? ', test_passed(2)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_qreal_aliases
