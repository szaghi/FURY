!< FURY test of [[qreal]].
program fury_test_qreal_conversions_alias
!< FURY test of [[qreal]]: conversion through the main alias of a unit.
use fury

type(system_si64) :: SI             !< SI system.
type(uom64)       :: lbf            !< Pound-force unit.
type(qreal64)     :: force          !< A force.
type(qreal64)     :: converted      !< A converted force.
type(uom64)       :: foot           !< Foot unit.
type(uom64)       :: kilometre      !< Kilometre unit.
logical           :: test_passed(3) !< List of passed tests.

test_passed = .false.

call SI%initialize
lbf = uom64('lbf< = 4.4482216152605 * N> [force] {pound_force}')

! "lbf" is converted into the main alias "N" of "kg.m.s-2 (N[force])"
force = qreal64(2._R8P, lbf)
converted = force%to(SI%unit('newton'))
test_passed(1) = abs(converted%magnitude - 8.896443230521_R8P) < 1e-12_R8P
print "(A,L1)", '2 lbf = '//converted%stringify(format='(F11.9)')//', is correct? ', test_passed(1)

! and back
call force%unset
force = converted%to(lbf)
test_passed(2) = abs(force%magnitude - 2._R8P) < 1e-12_R8P
print "(A,L1)", '8.896443231 N = '//force%stringify(format='(F11.9)')//', is correct? ', test_passed(2)

! through a common alias: "ft = 0.3048 * m" to "km = 1000 * m"
foot = uom64('ft< = 0.3048 * m> [length]')
kilometre = uom64('km = 1000 * m [length]')
call force%unset
call converted%unset
force = qreal64(35000._R8P, foot)
converted = force%to(kilometre)
test_passed(3) = abs(converted%magnitude - 10.668_R8P) < 1e-12_R8P
print "(A,L1)", '35000 ft = '//converted%stringify(format='(F6.3)')//', is correct? ', test_passed(3)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
if (.not.all(test_passed)) error stop 1
endprogram fury_test_qreal_conversions_alias
