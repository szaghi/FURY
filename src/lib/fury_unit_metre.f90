!< FURY definition of metre unit.
module fury_unit_metre
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY definition of metre unit.
!-----------------------------------------------------------------------------------------------------------------------------------
use fury_unit_abstract
use penf
! use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: unit_metre
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*), parameter   :: pre_defined_symbols = 'm,metre,metres,meter,meters' !< Pre-defined compatible symbols.
character(len=:), allocatable :: user_symbols                                        !< User-defined compatible symbols.

type, extends(unit_abstract) :: unit_metre
  !< Definition of metre unit.
  contains
    ! public methods
    procedure, pass(self) :: set           !< set the unit.
    procedure, pass(self) :: is_compatible !< Check if unit is compatible with another.
endtype unit_metre

interface unit_metre
  !< Ovearloading unit_metre name with a creator function.
  module procedure creator
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! non type bound procedures
  function creator(scale_factor, symbol) result(unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Create an instance of unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R_P),    intent(in) :: scale_factor !< Scale factor for multiple of base unit, e.g. 1000 for kilometres.
  character(*), intent(in) :: symbol       !< Litteral symbol of the unit, e.g. "m" for metres.
  type(unit_metre)         :: unit         !< The unit.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call unit%set(scale_factor=scale_factor, symbol=symbol)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction creator

  ! public methods
  subroutine set(self, scale_factor, symbol, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set the unit.
  !<
  !< @todo Load from file.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_metre), intent(inout)         :: self         !< The unit.
  real(R_P),         intent(in),  optional :: scale_factor !< Scale factor for multiple of base unit, e.g. 1000 for kilometres.
  character(*),      intent(in),  optional :: symbol       !< Litteral symbol of the unit, e.g. "m" for metres.
  integer(I_P),      intent(out), optional :: error        !< Error code, 0 => no errors happen.
  integer(I_P)                             :: error_       !< Error code, 0 => no errors happen, local variable.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error_ = 1
  self%scale_factor = 1._R_P
  if (present(scale_factor)) self%scale_factor = scale_factor
  self%symbol = 'm'
  if (present(symbol)) then
    self%symbol = symbol
    call update_compatible_user_symbols(symbol=symbol, pre_defined_symbols=pre_defined_symbols, user_symbols=user_symbols)
  endif
  error_ = 0
  if (present(error)) error = error_
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set

  elemental function is_compatible(self, unit) result(compatible)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if unit is compatible with another.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_metre),    intent(in) :: self       !< The unit.
  class(unit_abstract), intent(in) :: unit       !< The other unit.
  logical                          :: compatible !< Compatibility check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select type(unit)
  class is(unit_metre)
    ! check that both `self` and `unit` are in the same compatibility list
    compatible = (is_symbol_compatible(symbol=self%symbol, pre_defined_symbols=pre_defined_symbols, user_symbols=user_symbols) &
             .and.is_symbol_compatible(symbol=unit%symbol, pre_defined_symbols=pre_defined_symbols, user_symbols=user_symbols))
  class default
    compatible = .false.
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_compatible
endmodule fury_unit_metre
