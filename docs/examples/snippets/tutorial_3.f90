program tutorial_3
!< Tutorial 3: the algebra of units.
use fury
implicit none
type(uom64) :: metre, second, kilogram, pascal, pascal_symbol, force, force_from_symbol, speed, area, frequency

metre    = uom64('m [length]')
second   = uom64('s [time]')
kilogram = uom64('kg [mass]')

speed     = metre / second
area      = metre ** 2
frequency = second ** (-1)
print '(A)', 'speed     : '//speed%stringify(with_dimensions=.true.)
print '(A)', 'area      : '//area%stringify(with_dimensions=.true.)
print '(A)', 'frequency : '//frequency%stringify(with_dimensions=.true.)

pascal        = uom64('kg [mass].m-1 [length-1].s-2 [time-2] (Pa[pressure]) {pascal}')
pascal_symbol = uom64('Pa [pressure] {pascal}')
force             = pascal * metre ** 2
force_from_symbol = pascal_symbol * metre ** 2
print '(A)', 'pressure times area, from the references: '//force%stringify(with_dimensions=.true.)
print '(A)', 'pressure times area, from the symbol    : '//force_from_symbol%stringify(with_dimensions=.true.)
endprogram tutorial_3
