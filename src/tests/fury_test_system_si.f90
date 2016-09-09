!< FURY test of [[system_si]].
program fury_test_system_si
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY test of [[system_si]].
!-----------------------------------------------------------------------------------------------------------------------------------
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type(system_si64) :: SI             !< SI system.
type(uom64)       :: a_unit         !< A unit.
type(qreal64)     :: a_quantity     !< A quantity.
logical           :: test_passed(7) !< List of passed tests.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
test_passed = .true.

call SI%initialize
print "(A)", 'List of defined units in "'//SI%acronym//'" system:'
print "(A)", SI%list_units(with_dimensions=.true., with_aliases=.true., with_name=.true., compact_reals=.true.)
print "(A)", 'List of defined prefixes in "'//SI%acronym//'" system:'
print "(A)", SI%list_prefixes(with_aliases=.true., compact_reals=.true., prefix_string='  ')
print "(A)", 'List of defined constants in "'//SI%acronym//'" system:'
print "(A)", SI%list_constants(with_name=.true., compact_reals=.true., prefix_string='  ')

print "(A)", ''

a_unit = SI%unit('m')
test_passed(1) = a_unit == uom64('m')
print "(A,L1)", 'query "m" => '//a_unit%stringify(with_dimensions=.true., with_aliases=.true., with_name=.true.)//&
                ', is correct? ', test_passed(1)

call a_unit%unset
a_unit = SI%unit('metre')
test_passed(2) = a_unit == uom64('m')
print "(A,L1)", 'query "metre" => '//a_unit%stringify(with_dimensions=.true., with_aliases=.true., with_name=.true.)//&
                ', is correct? ', test_passed(2)

call a_unit%unset
a_unit = SI%unit('Pa')
test_passed(3) = a_unit == uom64('kg [mass].m-1 [length-1].s-2')
print "(A,L1)", 'query "Pa" => '//a_unit%stringify(with_dimensions=.true., with_aliases=.true., with_name=.true.)//&
                ', is correct? ', test_passed(3)

call a_unit%unset
a_unit = SI%unit('kilogram')
test_passed(4) = a_unit == uom64('kg')
print "(A,L1)", 'query "kilogram" => '//a_unit%stringify(with_dimensions=.true., with_aliases=.true., with_name=.true.)//&
                ', is correct? ', test_passed(4)

call a_unit%unset
a_unit = SI%unit('kg')
test_passed(5) = a_unit == uom64('kg')
print "(A,L1)", 'query "kg" => '//a_unit%stringify(with_dimensions=.true., with_aliases=.true., with_name=.true.)//&
                ', is correct? ', test_passed(5)

call a_unit%unset
a_unit = SI%unit('kilobyte')
test_passed(6) = a_unit == uom64('kilobyte')
print "(A,L1)", 'query "kilobyte" => '//a_unit%stringify(with_dimensions=.true., with_aliases=.true., protect_aliases=.true., &
                with_name=.true., compact_reals=.true.)//', is correct? ', test_passed(6)

call a_quantity%unset
a_quantity = SI%qunit('kilobyte')
a_quantity = 3.5 * a_quantity
test_passed(7) = a_quantity%magnitude == 3.5_R_P
print "(A,L1)", 'assigned 3.5 "kilobyte" => '//a_quantity%stringify(with_dimensions=.true., &
                compact_reals=.true.)//', is correct? ', test_passed(7)

print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
endprogram fury_test_system_si
