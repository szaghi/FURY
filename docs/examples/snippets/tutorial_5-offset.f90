kelvin  = uom64('K [temperature]')
celsius = uom64('degC< = 273.15 + K> [temperature]')
q = 36.6_R8P * celsius
converted = q%to(kelvin)
print '(A)', q%stringify(format='(F4.1)')//' = '//converted%stringify(format='(F6.2)')
