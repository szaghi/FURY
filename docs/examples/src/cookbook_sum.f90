program cookbook_sum
!< Cookbook: sum lengths given in different units.
use fury
implicit none
type(system_si64) :: SI
type(qreal64)     :: run, swim, bike, total

call SI%initialize
!region sum
swim = 3.8_R8P * SI%unit('km')
bike = 112._R8P * SI%unit('mi')
run  = 26.2_R8P * SI%unit('mi')
total = swim%to(SI%unit('m')) + bike%to(SI%unit('m')) + run%to(SI%unit('m'))
print '(A)', 'Ironman: '//total%stringify(format='(F8.1)')
!endregion sum
!run cookbook_sum cookbook_sum
endprogram cookbook_sum
