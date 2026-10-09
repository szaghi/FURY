dBm = uom64('dBm = @user mW')
mW  = uom64('mW')
call dBm%set_alias_conversion(reference_index=1, alias_index=2, convert=converter)

power = 20._R8P * dBm
converted = power%to(mW)
print '(A)', power%stringify(format='(F4.1)')//' = '//converted%stringify(format='(F5.1)')
call power%unset
power = converted%to(dBm)
print '(A)', converted%stringify(format='(F5.1)')//' = '//power%stringify(format='(F4.1)')
