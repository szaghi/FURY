!< FURY test of [[uom]].
program fury_test_uom_div
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[uom]].
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(uom64) :: si_length      !< SI length unit.
type(uom64) :: si_speed       !< SI speed unit.
type(uom64) :: si_time        !< SI time unit.
type(uom64) :: a_unit         !< A unit.
logical     :: test_passed(1) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

si_length = uom64(source='m [length]', name='metre')
si_time = uom64(source='s [time]', name='second')
si_speed = uom64(source='m [length].s-1 [time]', name='metre/second')
print "(A)", 'si_length = '//si_length%stringify(with_dimensions=.true.)
print "(A)", 'si_speed  = '//si_speed%stringify(with_dimensions=.true.)
print "(A)", 'si_time   = '//si_time%stringify(with_dimensions=.true.)
a_unit = si_length / si_time
test_passed(1) = a_unit == si_speed
print "(A,L1)", 'si_length/si_time = '//a_unit%stringify(with_dimensions=.true.)//', is correct? ', test_passed(1)
print "(A)", 'si_length/si_time name is: '//a_unit%name

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_uom_div
