program bolt
!< Quick start: how fast is Bolt?
use fury
implicit none
type(uom64)   :: metre, second
type(qreal64) :: distance, time, speed

!region units
metre  = uom64('m = metre = meter [length] {metre}')
second = uom64('s = sec = second [time] {second}')
!endregion units

!region quantities
distance = 100._R8P * metre
time     = 9.58_R8P * second
speed    = distance / time
!endregion quantities

print '(A)', 'distance : '//distance%stringify(format='(F6.2)')
print '(A)', 'time     : '//time%stringify(format='(F6.2)')
print '(A)', 'speed    : '//speed%stringify(format='(F6.3)', with_dimensions=.true.)
!run bolt bolt
endprogram bolt
