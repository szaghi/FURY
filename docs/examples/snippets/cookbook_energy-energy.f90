mass   = 1200._R8P * SI%unit('kg')
speed    = 100._R8P * SI%unit('km') / (1._R8P * SI%unit('hour'))
speed_si = speed%to(SI%unit('metre.second-1'))
energy   = 0.5_R8P * mass * speed_si**2
in_joule = energy%to(SI%unit('joule'))
print '(A)', 'speed : '//speed%stringify(format='(F5.1)')//' = '//speed_si%stringify(format='(F6.3)')
print '(A)', 'energy: '//in_joule%stringify(format='(F8.1)', with_aliases=.true.)
