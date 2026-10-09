program si_tables
!< Reference: the units, prefixes and constants of the SI system.
use fury
implicit none
type(system_si64) :: SI

call SI%initialize
print '(A)', SI%list_units(with_dimensions=.true., with_aliases=.true., with_name=.true., compact_reals=.true.)
print '(A)', SI%list_prefixes(with_aliases=.true., compact_reals=.true.)
print '(A)', SI%list_constants(with_dimensions=.true., with_name=.true., compact_reals=.true.)
endprogram si_tables
