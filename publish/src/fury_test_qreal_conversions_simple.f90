!< FURY test of [[qreal]].
program fury_test_qreal_conversions_simple
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[qreal]].
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(uom64)   :: kilometer      !< Kilometer unit.
type(uom64)   :: meter          !< Meter unit.
type(uom64)   :: kelvin         !< Kelvin unit.
type(uom64)   :: celsius        !< Celsius unit.
type(qreal64) :: q1             !< A quantity.
type(qreal64) :: q2             !< A quantity.
type(qreal64) :: q3             !< A quantity.
logical       :: test_passed(4) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

kilometer = uom64('km<=1000.0 * m>')
meter = uom64('m')

q1 = 3.1 * kilometer
q2 = q1%to(unit=meter)
test_passed(1) = q2%stringify(format='(F6.1)')=='3100.0 m'
print "(A,L1)", '3.1 km = '//q2%stringify(format='(F6.1)')//', is correct? ', test_passed(1)

q3 = q2%to(unit=kilometer)
test_passed(2) = q3%stringify(format='(F3.1)')=='3.1 km'
print "(A,L1)", '3100 m = '//q3%stringify(format='(F3.1)')//', is correct? ', test_passed(2)

call q1%unset
call q2%unset
call q3%unset

kelvin = uom64('K')
celsius = uom64('degC<=273.15 + K=celsius>')

q1 = 2 * kelvin
q2 = 1 * celsius
q3 = q1 + q2%to(kelvin)
test_passed(3) = q3%stringify(format='(F5.1)')=='276.1 K'
print "(A,L1)", '2 K + 1 celsius = '//q3%stringify(format='(F5.1)')//', is correct? ', test_passed(3)

call q3%unset
q3 = q1%to(celsius) + q2
test_passed(4) = q3%stringify(format='(F7.2)')=='-270.15 degC'
print "(A,L1)", '2 K + 1 celsius = '//q3%stringify(format='(F7.2)')//', is correct? ', test_passed(4)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_qreal_conversions_simple
