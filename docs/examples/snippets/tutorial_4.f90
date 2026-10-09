program tutorial_4
!< Tutorial 4: what FURY refuses, consistency checks.
use fury
implicit none
type(uom64)   :: metre, second, frequency
type(qreal64) :: length, other_length, time, total
character(16) :: scenario

metre  = uom64('m [length]')
second = uom64('s [time]')
length       = 3._R8P * metre
other_length = 2._R8P * metre
time         = 2._R8P * second
call get_command_argument(1, scenario)

select case(trim(scenario))
case('compare')
  print '(L1)', length == 3._R8P * metre
  print '(L1)', length /= other_length
  print '(L1)', length%has_same_unit(other_length)
  print '(L1)', length%has_same_unit(time)
case('add')
  total = length + time
case('assign')
  total = length
  total = time
case('reuse')
  total = length
  call total%unset
  total = time
  print '(A)', total%stringify(format='(F3.1)')
case('parse')
  frequency = uom64('Hz = s-1 [time-2]')
endselect
endprogram tutorial_4
