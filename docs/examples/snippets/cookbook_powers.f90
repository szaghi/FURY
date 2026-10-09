program cookbook_powers
!< Cookbook: powers and roots of quantities.
use fury
implicit none
type(uom64)   :: metre
type(qreal64) :: side, area, volume, root

metre = uom64('m [length]')
side   = 4._R8P * metre
area   = side ** 2
volume = side ** 3
root   = area ** 0.5_R8P
print '(A)', area%stringify(format='(F4.1)', with_dimensions=.true.)
print '(A)', volume%stringify(format='(F4.1)', with_dimensions=.true.)
print '(A)', root%stringify(format='(F3.1)', with_dimensions=.true.)
endprogram cookbook_powers
