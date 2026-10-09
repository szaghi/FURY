call aviation%initialize
print '(A)', aviation%list_units(with_dimensions=.true., with_aliases=.true., compact_reals=.true.)
altitude  = aviation%const('cruise_altitude')
converted = altitude%to(aviation%unit('km'))
print '(A)', altitude%stringify(format='(F7.1)')//' = '//converted%stringify(format='(F6.3)')
