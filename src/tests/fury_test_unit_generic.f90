!< FURY test of [[unit generic]].
program fury_test_unit_generic
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[unit generic]].
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(unit_generic), target :: si_length      !< SI length unit.
type(unit_generic), target :: si_speed       !< SI speed unit.
type(unit_generic), target :: si_time        !< SI time unit.
type(unit_generic), target :: a_unit         !< A unit.
logical                    :: test_passed(3) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

si_speed = unit_generic(scale_factor=1._R_P, symbols='m [length].s-1 [time]')
test_passed(1) = si_speed%stringify(with_dimensions=.true.)=='m.s-1 [length.time-1]'
print "(A,L1)", 'pass implicit dim. exp.:  m [length].s-1 [time] => '//si_speed%stringify(with_dimensions=.true.)//&
  ', is correct? ', test_passed(1)

print "(A)", ''
si_length = unit_generic(scale_factor=1._R_P, symbols='m [length]')
si_time = unit_generic(scale_factor=1._R_P, symbols='s [time]')
si_speed = unit_generic(scale_factor=1._R_P, symbols='m [length].s-1 [time]')
print "(A)", 'si_length = '//si_length%stringify(with_dimensions=.true.)
print "(A)", 'si_speed  = '//si_speed%stringify(with_dimensions=.true.)
print "(A)", 'si_time   = '//si_time%stringify(with_dimensions=.true.)
a_unit = si_length / si_time
test_passed(2) = a_unit%is_equal(other=si_speed)
print "(A,L1)", 'si_length / si_time = '//a_unit%stringify(with_dimensions=.true.)//', is correct? ', test_passed(2)

print "(A)", ''
print "(A)", 'An error should be raised'
call si_speed%unset
si_speed = unit_generic(scale_factor=1._R_P, symbols='m [length].s-1 [time2]') ! raise error
test_passed(3) = si_speed%stringify(with_dimensions=.true.)=='m.s-1 [length.time-1]'
print "(A,L1)", 'si_speed  = '//si_speed%stringify(with_dimensions=.true.)//', is correct? ', test_passed(3)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_unit_generic
