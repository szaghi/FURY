metre = uom64('m [length]')
mile  = uom64('mi< = 1609.344 * m> [length]')
q = 26.2_R8P * mile
converted = q%to(metre)
print '(A)', q%stringify(format='(F4.1)')//' = '//converted%stringify(format='(F7.1)')
