program cookbook_print
!< Cookbook: the string representation of a quantity.
use fury
implicit none
type(qreal64) :: g

g = qreal64(9.80665_R8P, uom64('m = metre [length].s-2 [time-2] (acc [acceleration]) {metre/second2}'), name='gravity')
print '(A)', g%stringify()
print '(A)', g%stringify(format='(F7.5)')
print '(A)', g%stringify(compact_reals=.true.)
print '(A)', g%stringify(compact_reals=.true., with_dimensions=.true.)
print '(A)', g%stringify(compact_reals=.true., with_aliases=.true.)
print '(A)', g%stringify(compact_reals=.true., with_name=.true.)
endprogram cookbook_print
