!< FURY test of [[qreal]].
program fury_test_xfail_qreal_to
!< FURY test of [[qreal]]: a conversion between inconsistent units stops the program.
use fury

type(uom64)   :: metre  !< Metre unit.
type(uom64)   :: second !< Second unit.
type(qreal64) :: length !< A length.
type(qreal64) :: time   !< A time.

metre = uom64('m [length]')
second = uom64('s [time]')
length = qreal64(1._R8P, metre)

print "(A)", 'An error will be raised (if all go right)'
time = length%to(second)

print "(A)", 'ERROR: the test should not reach this point, a previous error should have stop it before!'
endprogram fury_test_xfail_qreal_to
