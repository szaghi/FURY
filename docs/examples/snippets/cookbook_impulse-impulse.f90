impulse    = 10._R8P * (pound_force * second)
force      = impulse / (1._R8P * second)     ! back to a force, lbf
impulse_si = force%to(newton) * (1._R8P * second)
print '(A)', impulse%stringify(format='(F4.1)')//' = '//impulse_si%stringify(format='(F6.3)')
