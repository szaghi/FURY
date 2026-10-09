kilometre = SI%unit('km')
minute    = SI%unit('min')
distance  = 42.195_R8P * kilometre
converted = distance%to(SI%unit('m'))
print '(A)', distance%stringify(format='(F6.3)')//' = '//converted%stringify(format='(F7.1)')
