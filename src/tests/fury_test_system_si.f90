!< FURY test of [[system_si]].
program fury_test_system_si
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[system_si]].
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(system_si) :: system          !< SI system.
type(uom)       :: si_force        !< SI force unit.
type(uom)       :: si_length       !< SI length unit.
type(uom)       :: si_mass         !< SI mass unit.
type(uom)       :: si_speed        !< SI speed unit.
type(uom)       :: si_time         !< SI time unit.
type(uom)       :: a_unit          !< A unit.
logical         :: test_passed(10) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .true.

call system%initialize
print "(A)", 'List of defined units in "'//system%acronym//'" system:'
print "(A)", system%list_units(with_dimensions=.true., with_alias=.true.)

! si_speed = uom(symbols='m [length].s-1 [time]')
! test_passed(1) = si_speed%stringify(with_dimensions=.true.)=='m.s-1 [length.time-1]'
! print "(A,L1)", 'pass implicit dim. exp.:  m [length].s-1 [time] => '//si_speed%stringify(with_dimensions=.true.)//&
!                 ', is correct? ', test_passed(1)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_system_si
