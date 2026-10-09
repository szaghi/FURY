subroutine initialize(self, acronym)
class(system_aviation), intent(inout)        :: self
character(*),           intent(in), optional :: acronym

call self%free
self%acronym = 'AVIATION' ; if (present(acronym)) self%acronym = acronym
call self%add_unit('m = metre [length] {metre}')
call self%add_unit('s = second [time] {second}')
call self%add_unit('ft< = 0.3048 * m = foot> [length] {foot}')
call self%add_unit('NM = 1852 * m [length] {nautical_mile}')
call self%add_unit('h = 3600 * s = hour [time] {hour}')
call self%add_prefix('1.e3 * k = 1.e3 * kilo')
call self%add_constant(qreal64(magnitude=35000._R8P, unit=self%unit('ft'), name='cruise_altitude'))
endsubroutine initialize
