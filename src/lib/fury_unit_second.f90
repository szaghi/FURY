!< FURY definition of second unit.
module fury_unit_second
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY definition of second unit.
!-----------------------------------------------------------------------------------------------------------------------------------
use fury_unit_abstract
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: unit_second
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*), parameter   :: predefined_symbols = 's,second,seconds,sec,secs' !< Pre-defined compatible symbols.
character(len=:), allocatable :: user_symbols                                     !< User-defined compatible symbols.

type, extends(unit_abstract) :: unit_second
  !< Definition of second unit.
  contains
    ! public methods
    procedure, pass(self) :: is_compatible !< Check if unit is compatible with another.
    procedure, pass(self) :: set           !< set the unit.
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
  real(R_P),    intent(in) :: scale_factor !< Scale factor for multiple of base unit, e.g. 0.001 for milliseconds.
  character(*), intent(in) :: symbol       !< Litteral symbol of the unit, e.g. "s" for seconds.
  type(unit_second)        :: unit         !< The unit.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call unit%set(scale_factor=scale_factor, symbol=symbol)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction creator

  ! public methods
  elemental function is_compatible(self, unit) result(compatible)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if unit is compatible with another.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_second),   intent(in) :: self       !< The unit.
  class(unit_abstract), intent(in) :: unit       !< The other unit.
  logical                          :: compatible !< Compatibility check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select type(unit)
  class is(unit_second)
    ! check that both `self` and `unit` are in the same compatibility list
    compatible = (self%is_symbol_compatible(predefined_symbols=predefined_symbols, user_symbols=user_symbols) &
             .and.unit%is_symbol_compatible(predefined_symbols=predefined_symbols, user_symbols=user_symbols))
  class default
    compatible = .false.
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_compatible

  subroutine set(self, scale_factor, symbol, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set the unit.
  !<
  !< @todo Load from file.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_second), intent(inout)         :: self         !< The unit.
  real(R_P),          intent(in),  optional :: scale_factor !< Scale factor for multiple of base unit, e.g. 0.001 for milliseconds.
  character(*),       intent(in),  optional :: symbol       !< Litteral symbol of the unit, e.g. "s" for seconds.
  integer(I_P),       intent(out), optional :: error        !< Error code, 0 => no errors happen.
  integer(I_P)                              :: error_       !< Error code, 0 => no errors happen, local variable.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error_ = 1
  if (present(scale_factor)) self%scale_factor = scale_factor
  if (present(symbol)) then
    self%symbol = symbol
    call self%update_compatible_user_symbols(predefined_symbols=predefined_symbols, user_symbols=user_symbols)
  endif
  error_ = 0
  if (present(error)) error = error_
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set
endmodule fury_unit_second
