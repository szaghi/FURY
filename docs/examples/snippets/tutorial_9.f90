module aviation_system
!< Tutorial 9: a units system of your own, the units of aviation.
use fury
implicit none
private
public :: system_aviation

type, extends(system_abstract64) :: system_aviation
  contains
    procedure, pass(self) :: initialize
endtype system_aviation

contains
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
endmodule aviation_system

program tutorial_9
use aviation_system
use fury
implicit none
type(system_aviation) :: aviation
type(qreal64)         :: altitude, converted

call aviation%initialize
print '(A)', aviation%list_units(with_dimensions=.true., with_aliases=.true., compact_reals=.true.)
altitude  = aviation%const('cruise_altitude')
converted = altitude%to(aviation%unit('km'))
print '(A)', altitude%stringify(format='(F7.1)')//' = '//converted%stringify(format='(F6.3)')
endprogram tutorial_9
