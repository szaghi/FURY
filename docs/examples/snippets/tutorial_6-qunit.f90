kilobyte = 64 * SI%qunit('KiB')
call converted%unset
converted = kilobyte%to(SI%unit('byte'))
print '(A)', kilobyte%stringify(format='(F4.1)')//' = '//converted%stringify(format='(F7.1)')
