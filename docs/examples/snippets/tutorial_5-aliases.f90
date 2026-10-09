second = uom64('s [time]')
hour   = uom64('h = 3600 * s = hr [time]')
q = 1.5_R8P * hour
converted = q%to(second)
print '(A)', q%stringify(format='(F3.1)')//' = '//converted%stringify(format='(F6.1)')
call q%unset
q = converted%to(hour)
print '(A)', converted%stringify(format='(F6.1)')//' = '//q%stringify(format='(F3.1)')
