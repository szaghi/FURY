newton      = uom64('kg [mass].m [length].s-2 [time-2] (N[force]) {newton}')
pound_force = uom64('lbf< = 4.4482216152605 * N> [force] {pound_force}')
q = 10._R8P * pound_force
converted = q%to(newton)
print '(A)', q%stringify(format='(F4.1)')//' = '//converted%stringify(format='(F6.3)', with_aliases=.true.)
