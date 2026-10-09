program tutorial_2
!< Tutorial 2: defining units, the grammar of a unit definition.
use fury
implicit none
type(uom64) :: symbol_only, with_dimensions, with_aliases, with_name, compound, with_main_alias

!region define
symbol_only     = uom64('m')
with_dimensions = uom64('m [length]')
with_aliases    = uom64('m = metre = meter [length]')
with_name       = uom64('m = metre = meter [length] {metre}')
compound        = uom64('kg [mass].m [length].s-2 [time-2]')
with_main_alias = uom64('kg [mass].m [length].s-2 [time-2] (N[force]) {newton}')
!endregion define

!region print
print '(A)', symbol_only%stringify(with_dimensions=.true.)
print '(A)', with_dimensions%stringify(with_dimensions=.true.)
print '(A)', with_aliases%stringify(with_dimensions=.true., with_aliases=.true.)
print '(A)', with_name%stringify(with_dimensions=.true., with_aliases=.true., with_name=.true.)
print '(A)', compound%stringify(with_dimensions=.true.)
print '(A)', with_main_alias%stringify(with_dimensions=.true., with_aliases=.true., with_name=.true.)
!endregion print
!run tutorial_2 tutorial_2
endprogram tutorial_2
