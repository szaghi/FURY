program cookbook_temperature
!< Cookbook: temperatures, conversions with an offset and through a common unit.
use fury
implicit none
type(uom64)   :: kelvin, celsius, fahrenheit
type(qreal64) :: body, in_celsius, in_kelvin

kelvin     = uom64('K [temperature]')
celsius    = uom64('degC< = 273.15 + K> [temperature]')
fahrenheit = uom64('degF< = 255.37222222222223 + 0.5555555555555556 * K> [temperature]')

body       = 98.6_R8P * fahrenheit
in_celsius = body%to(celsius)       ! through K, the alias the two units share
in_kelvin  = body%to(kelvin)
print '(A)', body%stringify(format='(F4.1)')//' = '//in_celsius%stringify(format='(F5.2)')//' = '//&
             in_kelvin%stringify(format='(F6.2)')
endprogram cookbook_temperature
