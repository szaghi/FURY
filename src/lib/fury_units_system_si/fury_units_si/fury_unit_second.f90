!< FURY definition of second unit.
module fury_unit_second
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY definition of second unit.
!-----------------------------------------------------------------------------------------------------------------------------------
use fury_unit_time
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: unit_second
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, extends(unit_time) :: unit_second
  !< Definition of second unit.
  contains
    ! public overrided methods
    procedure, pass(self) :: set !< set the unit.
endtype unit_second

interface unit_second
  !< Ovearloading unit_second name with a creator function.
  module procedure creator
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! non type bound procedures
  function creator(scale_factor, symbol) result(unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Create an instance of unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R_P),    intent(in), optional :: scale_factor !< Scale factor for multiple of base unit, e.g. 0.001 for milliseconds.
  character(*), intent(in), optional :: symbol       !< Litteral symbol of the unit, e.g. "s" for seconds.
  type(unit_second)                  :: unit         !< The unit.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call unit%set(scale_factor=scale_factor, symbol=symbol, dimensionality=unit%dimensionality_)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction creator

  ! public overrided methods
  subroutine set(self, scale_factor, symbol, dimensionality, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set the unit.
  !<
  !< @todo Load from file.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_second), intent(inout)         :: self           !< The unit.
  real(R_P),          intent(in),  optional :: scale_factor   !< Scale factor for multiple of base unit, e.g. 0.001 for millisecs.
  character(*),       intent(in),  optional :: symbol         !< Litteral symbol of the unit, e.g. "s" for seconds.
  character(*),       intent(in),  optional :: dimensionality !< Reference dimensionality symbol, e.g. "[time]" for seconds.
  integer(I_P),       intent(out), optional :: error          !< Error code, 0 => no errors happen.
  integer(I_P)                              :: error_         !< Error code, 0 => no errors happen, local variable.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error_ = 1
  self%scale_factor = 1._R_P ;if (present(scale_factor)) self%scale_factor = scale_factor
  self%symbol = 's' ; if (present(symbol)) self%symbol = symbol
  self%dimensionality = '[time]' ; if (present(dimensionality)) self%dimensionality = dimensionality
  error_ = 0
  if (present(error)) error = error_
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set
endmodule fury_unit_second
