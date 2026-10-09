program tutorial_5
!< Tutorial 5: conversions, aliases with a factor or an offset.
use fury
implicit none
type(uom64)   :: metre, kilometre, mile, kelvin, celsius, second, hour, newton, pound_force
type(qreal64) :: q, converted
character(16) :: scenario

call get_command_argument(1, scenario)
select case(trim(scenario))
case('factor')
  metre     = uom64('m [length]')
  kilometre = uom64('km = 1000 * m [length]')
  q = 3.2_R8P * kilometre
  converted = q%to(metre)
  print '(A)', q%stringify(format='(F3.1)')//' = '//converted%stringify(format='(F6.1)')
case('protected')
  metre = uom64('m [length]')
  mile  = uom64('mi< = 1609.344 * m> [length]')
  q = 26.2_R8P * mile
  converted = q%to(metre)
  print '(A)', q%stringify(format='(F4.1)')//' = '//converted%stringify(format='(F7.1)')
case('offset')
  kelvin  = uom64('K [temperature]')
  celsius = uom64('degC< = 273.15 + K> [temperature]')
  q = 36.6_R8P * celsius
  converted = q%to(kelvin)
  print '(A)', q%stringify(format='(F4.1)')//' = '//converted%stringify(format='(F6.2)')
case('aliases')
  second = uom64('s [time]')
  hour   = uom64('h = 3600 * s = hr [time]')
  q = 1.5_R8P * hour
  converted = q%to(second)
  print '(A)', q%stringify(format='(F3.1)')//' = '//converted%stringify(format='(F6.1)')
  call q%unset
  q = converted%to(hour)
  print '(A)', converted%stringify(format='(F6.1)')//' = '//q%stringify(format='(F3.1)')
case('main-alias')
  newton      = uom64('kg [mass].m [length].s-2 [time-2] (N[force]) {newton}')
  pound_force = uom64('lbf< = 4.4482216152605 * N> [force] {pound_force}')
  q = 10._R8P * pound_force
  converted = q%to(newton)
  print '(A)', q%stringify(format='(F4.1)')//' = '//converted%stringify(format='(F6.3)', with_aliases=.true.)
case('impossible')
  metre  = uom64('m [length]')
  second = uom64('s [time]')
  q = 3._R8P * metre
  converted = q%to(second)
endselect
endprogram tutorial_5
