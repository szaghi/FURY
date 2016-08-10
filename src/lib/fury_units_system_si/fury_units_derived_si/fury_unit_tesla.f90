!< FURY definition of tesla unit.
module fury_unit_tesla
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY definition of tesla unit.
!-----------------------------------------------------------------------------------------------------------------------------------
use fury_unit_magnetic_flux_density
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: unit_tesla
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, extends(unit_magnetic_flux_density) :: unit_tesla
  !< Definition of tesla unit.
  contains
    ! public overrided methods
    procedure, pass(self) :: set !< set the unit.
endtype unit_tesla

interface unit_tesla
  !< Ovearloading unit_tesla name with a creator function.
  module procedure creator
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! non type bound procedures
  function creator(scale_factor, symbol, dimensionality) result(unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Create an instance of unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R_P),    intent(in), optional :: scale_factor   !< Scale factor for multiple of base unit, e.g. 1000 for kilometres.
  character(*), intent(in), optional :: symbol         !< Litteral symbol of the unit, e.g. "m" for metres.
  character(*), intent(in), optional :: dimensionality !< Reference dimensionality symbol, e.g. "[length]" for metres.
  type(unit_tesla)                   :: unit           !< The unit.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call unit%set(scale_factor=scale_factor, symbol=symbol, dimensionality=dimensionality)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction creator

  ! public overrided methods
  subroutine set(self, scale_factor, symbol, dimensionality, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set the unit.
  !<
  !< @todo Load from file.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_tesla), intent(inout)         :: self           !< The unit.
  real(R_P),         intent(in),  optional :: scale_factor   !< Scale factor for multiple of base unit, e.g. 1000 for km.
  character(*),      intent(in),  optional :: symbol         !< Litteral symbol of the unit, e.g. "m" for metres.
  character(*),      intent(in),  optional :: dimensionality !< Reference dimensionality symbol, e.g. "[length]" for metres.
  integer(I_P),      intent(out), optional :: error          !< Error code, 0 => no errors happen.
  integer(I_P)                             :: error_         !< Error code, 0 => no errors happen, local variable.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error_ = 1
  self%scale_factor = 1._R_P ; if (present(scale_factor)) self%scale_factor = scale_factor
  self%symbol = 'T' ; if (present(symbol)) self%symbol = symbol
  self%dimensionality = '[mass]/[time]/[time]/[current]' ; if (present(dimensionality)) self%dimensionality = dimensionality
  error_ = 0
  if (present(error)) error = error_
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set
endmodule fury_unit_tesla
