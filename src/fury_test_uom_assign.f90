!< FURY test of [[uom]].
program fury_test_uom_assign
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[uom]].
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(uom64) :: si_speed       !< SI speed unit.
type(uom64) :: a_unit         !< A unit.
logical     :: test_passed(4) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

si_speed = uom64(source='m [length].s-1 [time]', name='si_speed')
test_passed(1) = si_speed%stringify(with_dimensions=.true.)=='m.s-1 [length.time-1]'
print "(A,L1)", 'pass implicit dim. exp.:  m [length].s-1 [time] => '//si_speed%stringify(with_dimensions=.true.)//&
                ', is correct? ', test_passed(1)

call si_speed%unset
si_speed = uom64(source='m [length].s-1 [time-1]', name='si_speed')
test_passed(2) = si_speed%stringify(with_dimensions=.true.)=='m.s-1 [length.time-1]'
print "(A,L1)", 'pass explicit dim. exp.:  m [length].s-1 [time-1] => '//si_speed%stringify(with_dimensions=.true.)//&
                ', is correct? ', test_passed(2)

call si_speed%unset
si_speed = uom64('m [length].s-1 [time-1]{si_speed}')
test_passed(3) = si_speed%stringify(with_dimensions=.true.)=='m.s-1 [length.time-1]'
print "(A,L1)", 'pass as string:  m [length].s-1 [time-1]{si_speed} => '//si_speed%stringify(with_dimensions=.true.)//&
                ', is correct? ', test_passed(3)

a_unit = si_speed
test_passed(4) = a_unit == si_speed
print "(A,L1)", 'assign to other unit = si_speed => '//si_speed%stringify(with_dimensions=.true.)//', is correct? ', test_passed(4)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_uom_assign
