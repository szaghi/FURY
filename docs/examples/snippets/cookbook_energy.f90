program cookbook_energy
!< Cookbook: compute with the SI system, the kinetic energy of a car.
use fury
implicit none
type(system_si64) :: SI
type(qreal64)     :: mass, speed, speed_si, energy, in_joule

call SI%initialize
mass   = 1200._R8P * SI%unit('kg')
speed    = 100._R8P * SI%unit('km') / (1._R8P * SI%unit('hour'))
speed_si = speed%to(SI%unit('metre.second-1'))
energy   = 0.5_R8P * mass * speed_si**2
in_joule = energy%to(SI%unit('joule'))
print '(A)', 'speed : '//speed%stringify(format='(F5.1)')//' = '//speed_si%stringify(format='(F6.3)')
print '(A)', 'energy: '//in_joule%stringify(format='(F8.1)', with_aliases=.true.)
endprogram cookbook_energy
