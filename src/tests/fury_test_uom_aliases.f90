!< FURY test of [[uom]].
program fury_test_uom_aliases
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[uom]].
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(uom) :: si_frequency   !< SI frequency unit.
type(uom) :: si_pressure    !< SI pressure unit.
type(uom) :: a_unit         !< A unit.
logical   :: test_passed(4) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .false.

a_unit = 'Pa[pressure] {pascal}'
si_pressure = 'kg [mass].m-1 [length-1].s-2 [time-2] (Pa[pressure]) {pascal}'
test_passed(1) = a_unit == si_pressure
print "(A,L1)", 'Pa = '//si_pressure%stringify()//', is correct? ', test_passed(1)

test_passed(2) = si_pressure%has_reference(reference=a_unit%references(1))
print "(A,L1)", si_pressure%stringify(with_alias=.true.)//' has symbol Pa, is correct? ', test_passed(2)

si_frequency = 'Hz [frequency]'
call a_unit%unset
a_unit = uom('s-1 = Hz = hertz [frequency]') / si_frequency
test_passed(3) = a_unit%stringify()=='s0'
print "(A,L1)", 's-1/Hz = '//a_unit%stringify()//', is correct? ', test_passed(3)

call a_unit%unset
a_unit = si_frequency / uom('s-1 = Hz = hertz [frequency]')
test_passed(4) = a_unit%stringify()=='Hz0'
print "(A,L1)", 'Hz/s-1 = '//a_unit%stringify()//', is correct? ', test_passed(4)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_uom_aliases
