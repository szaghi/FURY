!< FURY test of [[qreal]].
program fury_test_qreal_conversions_si
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[qreal]].
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(uom)       :: kilometer      !< Kilometer unit.
type(uom)       :: meter          !< Meter unit.
type(uom)       :: kHz            !< Kilohertz unit.
type(uom)       :: Hz             !< Hertz unit.
type(uom)       :: kelvin         !< Kelvin unit.
type(uom)       :: celsius        !< Celsius unit.
type(uom)       :: second         !< Second unit.
type(qreal)     :: q1             !< A quantity.
type(qreal)     :: q2             !< A quantity.
type(qreal)     :: q3             !< A quantity.
type(system_si) :: SI             !< SI system.
logical         :: test_passed(4) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

call SI%initialize

kilometer = SI%unit('kilometre')
meter     = SI%unit('metre')
kHz       = SI%unit('kilohertz')
Hz        = SI%unit('hertz')
second    = SI%unit('second')
kelvin    = SI%unit('kelvin')
celsius   = SI%unit('celsius')

call q1%unset
call q2%unset
call q3%unset
q1 = 2 * kilometer
q2 = 1 * meter
q3 = q1%to(meter) + q2
test_passed(1) = q3%stringify(format='(F6.1)')=='2001.0 m'
print "(A,L1)", '2.0 km + 1.0 m = '//q3%stringify(format='(F6.1)')//', is correct? ', test_passed(1)

call q1%unset
call q2%unset
call q3%unset
q1 = 2 * kHz
q2 = 1 * Hz
q3 = q1%to(Hz) + q2
test_passed(2) = q3%stringify(format='(F6.1)')=='2001.0 Hz'
print "(A,L1)", '2.0 kHz + 1.0 Hz = '//q3%stringify(format='(F6.1)')//', is correct? ', test_passed(2)

call q1%unset
call q2%unset
call q3%unset
q1 = 2 * kHz
q2 = 1 * Hz
q3 = q1%to(second**(-1)) + q2%to(second**(-1))
test_passed(3) = q3%stringify(format='(F6.1)')=='2001.0 s-1'
print "(A,L1)", '2.0 ks-1 + 1.0 Hz = '//q3%stringify(format='(F6.1)')//', is correct? ', test_passed(3)

call q1%unset
call q2%unset
call q3%unset
q1 = 2 * celsius
q2 = 1 * kelvin
q3 = q1%to(kelvin) + q2
test_passed(4) = q3%stringify(format='(F6.2)')=='276.15 K'
print "(A,L1)", '2.0 celsius + 1.0 K = '//q3%stringify(format='(F6.2)')//', is correct? ', test_passed(4)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_qreal_conversions_si
