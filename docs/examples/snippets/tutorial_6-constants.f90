light = SI%const('speed_of_light')
g     = SI%const('gravity')
weight = 75._R8P * SI%unit('kg') * g
print '(A)', light%stringify(compact_reals=.true., with_name=.true.)
print '(A)', 'weight: '//weight%stringify(format='(F5.1)', with_dimensions=.true.)
