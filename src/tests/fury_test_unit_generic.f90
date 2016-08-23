!< FURY test of [[unit generic]].
program fury_test_unit_generic
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[unit generic]].
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(unit_generic) :: si_force       !< SI force unit.
type(unit_generic) :: si_length      !< SI length unit.
type(unit_generic) :: si_mass        !< SI mass unit.
type(unit_generic) :: si_speed       !< SI speed unit.
type(unit_generic) :: si_time        !< SI time unit.
type(unit_generic) :: a_unit         !< A unit.
logical            :: test_passed(4) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

si_speed = unit_generic(symbols='m [length].s-1 [time]')
test_passed(1) = si_speed%stringify(with_dimensions=.true.)=='m.s-1 [length.time-1]'
print "(A,L1)", 'pass implicit dim. exp.:  m [length].s-1 [time] => '//si_speed%stringify(with_dimensions=.true.)//&
                ', is correct? ', test_passed(1)
call si_speed%unset
si_speed = unit_generic(symbols='m [length].s-1 [time-1]')
test_passed(2) = si_speed%stringify(with_dimensions=.true.)=='m.s-1 [length.time-1]'
print "(A,L1)", 'pass explicit dim. exp.:  m [length].s-1 [time-1] => '//si_speed%stringify(with_dimensions=.true.)//&
                ', is correct? ', test_passed(2)

print "(A)", ''
print "(A)", 'Test unit/unit'
call si_speed%unset
si_length = unit_generic(symbols='m [length]', name='metre')
si_time = unit_generic(symbols='s [time]', name='second')
si_speed = unit_generic(symbols='m [length].s-1 [time]', name='metre/second')
print "(A)", 'si_length = '//si_length%stringify(with_dimensions=.true.)
print "(A)", 'si_speed  = '//si_speed%stringify(with_dimensions=.true.)
print "(A)", 'si_time   = '//si_time%stringify(with_dimensions=.true.)
a_unit = si_length / si_time
test_passed(3) = a_unit%is_equal(other=si_speed)
print "(A,L1)", 'si_length/si_time = '//a_unit%stringify(with_dimensions=.true.)//', is correct? ', test_passed(3)
print "(A)", 'si_length/si_time name is: '//a_unit%name

print "(A)", ''
print "(A)", 'Test unit*unit'
si_force = unit_generic(symbols='kg [mass].m [length].s-2 [time-2]', name='newton')
si_mass = unit_generic(symbols='kg [mass]', name='kilogram')
print "(A)", 'si_force = '//si_force%stringify(with_dimensions=.true.)
print "(A)", 'si_mass = '//si_mass%stringify(with_dimensions=.true.)
call a_unit%unset
a_unit = si_mass * si_length / si_time / si_time
test_passed(4) = a_unit%is_equal(other=si_force)
print "(A,L1)", 'si_mass*si_length/si_time/si_time = '//a_unit%stringify(with_dimensions=.true.)//', is correct? ', test_passed(4)
print "(A)", 'si_mass*si_length/si_time/si_time name is: '//a_unit%name

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_unit_generic
