metre  = SI%unit('metre')
newton = SI%unit('N')
print '(A)', metre%stringify(with_dimensions=.true., with_aliases=.true., with_name=.true.)
print '(A)', newton%stringify(with_dimensions=.true., with_aliases=.true., with_name=.true.)
