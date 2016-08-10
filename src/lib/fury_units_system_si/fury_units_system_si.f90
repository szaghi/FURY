!< FURY definition of units symbols of *International System of Units*.
module fury_units_system_si
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY definition of units symbols of *International System of Units*.
!-----------------------------------------------------------------------------------------------------------------------------------
use fury_unit_abstract
use fury_unit_metre
use fury_unit_second
use fury_unit_metre_per_second
use fury_unit_metre_square
use fury_units_system_abstract
use penf
! use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: units_system_si
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, extends(units_system_abstract) :: units_system_si
  type(unit_metre)            :: metre            !< The metre unit instance.
  type(unit_second)           :: second           !< The second unit instance.
  type(unit_metre_per_second) :: metre_per_second !< The metre/second unit instance.
  type(unit_metre_square)     :: metre_square     !< The metre**2 unit instance.
  contains
    ! public deferred methods
    procedure, pass(self) :: associate_unit !< Associate unit by dimensionality.
    procedure, pass(self) :: initialize     !< Initialize the units system.
endtype units_system_si
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  subroutine associate_unit(self, dimensionality, unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Associate unit by dimensionality.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(units_system_si), intent(in), target   :: self           !< The units system.
  character(*),           intent(in)           :: dimensionality !< Reference dimensionality symbol, e.g. "[length]" for m.
  class(unit_abstract),   intent(out), pointer :: unit           !< Unit of measure of quantity.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select case(dimensionality)
  case('[length]')
    unit => self%metre
  case('[time]')
    unit => self%second
  case('[length]/[time]')
    unit => self%metre_per_second
  case('[length]*[length]')
    unit => self%metre_square
  case default
    unit => null()
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine associate_unit

  subroutine initialize(self, acronym, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Initialize the units system.
  !<
  !< @todo Load from file.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(units_system_si), intent(inout)         :: self    !< The units system.
  character(*),           intent(in),  optional :: acronym !< Units system acronym, e.g. "SI" for the International System.
  integer(I_P),           intent(out), optional :: error   !< Error code, 0 => no errors happen.
  integer(I_P)                                  :: error_  !< Error code, 0 => no errors happen, local variable.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error_ = 0
  self%acronym = 'SI' ; if (present(acronym)) self%acronym = acronym
  call self%metre%set(scale_factor=1._R_P, symbol='m', dimensionality='[length]', error=error_)
  call self%second%set(scale_factor=1._R_P, symbol='s', dimensionality='[time]', error=error_)
  call self%metre_per_second%set(scale_factor=1._R_P, symbol='m/s', dimensionality='[length]/[time]', error=error_)
  call self%metre_square%set(scale_factor=1._R_P, symbol='m**2', dimensionality='[length]*[length]', error=error_)
  if (present(error)) error = error_
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine initialize
endmodule fury_units_system_si
