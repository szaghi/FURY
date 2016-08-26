!< FURY test of [[uom]].
program fury_test_uom_mul
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[uom]].
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(uom) :: si_force       !< SI force unit.
type(uom) :: si_length      !< SI length unit.
type(uom) :: si_mass        !< SI mass unit.
type(uom) :: si_time        !< SI time unit.
type(uom) :: a_unit         !< A unit.
logical   :: test_passed(1) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

si_force = uom(source='kg [mass].m [length].s-2 [time-2]', name='newton')
si_length = uom(source='m [length]', name='metre')
si_mass = uom(source='kg [mass]', name='kilogram')
si_time = uom(source='s [time]', name='second')
print "(A)", 'si_force  = '//si_force%stringify(with_dimensions=.true.)
print "(A)", 'si_mass   = '//si_mass%stringify(with_dimensions=.true.)
print "(A)", 'si_length = '//si_length%stringify(with_dimensions=.true.)
print "(A)", 'si_time   = '//si_time%stringify(with_dimensions=.true.)
a_unit = si_mass * si_length / si_time / si_time
test_passed(1) = a_unit == si_force
print "(A,L1)", 'si_mass*si_length/si_time/si_time = '//a_unit%stringify(with_dimensions=.true.)//', is correct? ', test_passed(1)
print "(A)", 'si_mass*si_length/si_time/si_time name is: '//a_unit%name

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_uom_mul
