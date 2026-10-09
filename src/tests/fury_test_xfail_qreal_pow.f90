!< FURY test of [[qreal]].
program fury_test_xfail_qreal_pow
!< FURY test of [[qreal]]: a real power giving a non integer exponent of a unit stops the program.
use fury

type(uom64)   :: metre  !< Metre unit.
type(qreal64) :: length !< A length.
type(qreal64) :: root   !< Its square root.

metre = uom64('m [length]')
length = 2._R8P * metre

print "(A)", 'An error will be raised (if all go right)'
root = length ** 0.5_R8P

print "(A)", 'ERROR: the test should not reach this point, a previous error should have stop it before!'
endprogram fury_test_xfail_qreal_pow
