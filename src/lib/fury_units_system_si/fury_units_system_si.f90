!< FURY definition of units symbols of *International System of Units*.
module fury_units_system_si
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY definition of units symbols of *International System of Units*.
!-----------------------------------------------------------------------------------------------------------------------------------
use fury_unit_abstract
! units base
use fury_unit_ampere
use fury_unit_candela
use fury_unit_kelvin
use fury_unit_kilogram
use fury_unit_metre
use fury_unit_mole
use fury_unit_second
! units derived
use fury_unit_metre_per_second
use fury_unit_metre_square

use fury_units_system_abstract
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: units_system_si
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, extends(units_system_abstract) :: units_system_si
  ! units base
  type(unit_ampere)   :: ampere   !< The ampere unit instance.
  type(unit_candela)  :: candela  !< The candela unit instance.
  type(unit_kelvin)   :: kelvin   !< The kelvin unit instance.
  type(unit_kilogram) :: kilogram !< The kilogram unit instance.
  type(unit_metre)    :: metre    !< The metre unit instance.
  type(unit_mole)     :: mole     !< The mole unit instance.
  type(unit_second)   :: second   !< The second unit instance.
  ! units derived
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
  ! units base
  case('[current]')
    unit => self%ampere
  case('[luminosity]')
    unit => self%candela
  case('[temperature]')
    unit => self%kelvin
  case('[mass]')
    unit => self%kilogram
  case('[length]')
    unit => self%metre
  case('[substance]')
    unit => self%mole
  case('[time]')
    unit => self%second
  ! units derived
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
  ! units base
  call self%ampere%set(scale_factor=1._R_P, symbol='A', dimensionality='[current]', error=error_)
  call self%candela%set(scale_factor=1._R_P, symbol='cd', dimensionality='[luminosity]', error=error_)
  call self%kelvin%set(scale_factor=1._R_P, symbol='K', dimensionality='[temperature]', error=error_)
  call self%kilogram%set(scale_factor=1._R_P, symbol='kg', dimensionality='[mass]', error=error_)
  call self%metre%set(scale_factor=1._R_P, symbol='m', dimensionality='[length]', error=error_)
  call self%mole%set(scale_factor=1._R_P, symbol='mol', dimensionality='[substance]', error=error_)
  call self%second%set(scale_factor=1._R_P, symbol='s', dimensionality='[time]', error=error_)
  ! units derived
  call self%metre_per_second%set(scale_factor=1._R_P, symbol='m/s', dimensionality='[length]/[time]', error=error_)
  call self%metre_square%set(scale_factor=1._R_P, symbol='m**2', dimensionality='[length]*[length]', error=error_)
  if (present(error)) error = error_
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine initialize
endmodule fury_units_system_si
