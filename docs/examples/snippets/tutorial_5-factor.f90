metre     = uom64('m [length]')
kilometre = uom64('km = 1000 * m [length]')
q = 3.2_R8P * kilometre
converted = q%to(metre)
print '(A)', q%stringify(format='(F3.1)')//' = '//converted%stringify(format='(F6.1)')
