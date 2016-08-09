!< FURY definition of abstract unit class.
module fury_unit_abstract
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY definition of abstract unit class.
!<
!<### Units compatibility
!<
!< Units are claimed to be *compatible* (e.g. quantities with these units can be summed/subtracted) if their symbols are defined
!< into the same *compatibility list*.
!<
!<#### Compatibility list
!<
!< The compatibility list is made by two different parts:
!<
!<1. a predefined list, that is a character parameter variable containing a comma separated list of symbols, e.g. for meters
!< we could have "m,metre,metres,meter,meters";
!<2. a user list, that is a character deferred length variable containing a comma separated list of symbols, e.g. for meters
!< we could have "m,metre,metres,meter,meters";
!<
!< The full list is the sum of the above two. These lists are defined into the concrete units modules, e.g.
!< see [[fury_unit_metre]].
!-----------------------------------------------------------------------------------------------------------------------------------
use penf
use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: unit_abstract
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, abstract :: unit_abstract
  !< Abstract prototype of *unit*.
  real(R_P)                     :: scale_factor !< Scale factor for multiple of base unit, e.g. 1000 for kilometres.
  character(len=:), allocatable :: symbol       !< Litteral symbol(s) of the unit, e.g. "m" for metres.
  contains
    ! public deferred methods
    procedure(is_compatible_interface), pass(self), deferred :: is_compatible !< Check if unit is compatible with another.
    procedure(set_interface),           pass(self), deferred :: set           !< Set the unit.
    ! public methods
    procedure, nopass     :: clear_user_symbols             !< Clear user-defined compatible symbols.
    procedure, pass(self) :: is_symbol_compatible           !< Check is the unit symbol is into the compatible list.
    procedure, pass(self) :: update_compatible_user_symbols !< Update user-defined compatible symbols list.
endtype unit_abstract

abstract interface
  !< Check if unit is compatible with another.
  elemental function is_compatible_interface(self, unit) result(compatible)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if unit is compatible with another.
  !---------------------------------------------------------------------------------------------------------------------------------
  import unit_abstract
  class(unit_abstract), intent(in) :: self       !< The unit.
  class(unit_abstract), intent(in) :: unit       !< The other unit.
  logical                          :: compatible !< Compatibility check result.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_compatible_interface
endinterface

abstract interface
  !< Initialize the unit.
  subroutine set_interface(self, scale_factor, symbol, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set units definition.
  !---------------------------------------------------------------------------------------------------------------------------------
  import I_P, R_P, unit_abstract
  class(unit_abstract), intent(inout)         :: self         !< The unit.
  real(R_P),            intent(in), optional  :: scale_factor !< Scale factor for multiple of base unit, e.g. 1000 for kilometres.
  character(*),         intent(in), optional  :: symbol       !< Litteral symbol of the unit, e.g. "m" for metres.
  integer(I_P),         intent(out), optional :: error        !< Error code, 0 => no errors happen.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set_interface
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! public methods
  subroutine clear_user_symbols(user_symbols)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Clear user-defined compatible symbols.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(len=:), allocatable, intent(inout) :: user_symbols !< User-defined compatible symbols.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(user_symbols)) deallocate(user_symbols)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine clear_user_symbols

  pure function is_symbol_compatible(self, predefined_symbols, user_symbols) result(is_compatible)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check is the unit symbol is into the compatible list.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_abstract),          intent(in) :: self               !< The unit.
  character(*),                  intent(in) :: predefined_symbols !< Pre-defined compatible symbols.
  character(len=:), allocatable, intent(in) :: user_symbols       !< User-defined compatible symbols.
  logical                                   :: is_compatible      !< Result of compatibility check.
  type(string), allocatable                 :: symbols_tk(:)      !< Compatible symbols tokenized.
  integer(I_P)                              :: s                  !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_compatible = .false.
  call get_compatible_symbols_tokens(predefined_symbols=predefined_symbols, user_symbols=user_symbols, symbols_tk=symbols_tk)
  do s=1, size(symbols_tk, dim=1)
    if (symbols_tk(s)==self%symbol) then
      is_compatible = .true.
      exit
    endif
  enddo
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_symbol_compatible

  pure subroutine update_compatible_user_symbols(self, predefined_symbols, user_symbols)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Update user-defined compatible symbols list.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_abstract),          intent(in)    :: self               !< The unit.
  character(*),                  intent(in)    :: predefined_symbols !< Pre-defined compatible symbols.
  character(len=:), allocatable, intent(inout) :: user_symbols       !< User-defined compatible symbols.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (.not.self%is_symbol_compatible(predefined_symbols=predefined_symbols, user_symbols=user_symbols)) then
    if (allocated(user_symbols)) then
      user_symbols = user_symbols//','//self%symbol
    else
      user_symbols = self%symbol
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine update_compatible_user_symbols

  ! private non type bound procedures
  pure subroutine get_compatible_symbols_tokens(predefined_symbols, user_symbols, symbols_tk)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the list of compatible symbols tokenized considering both pre-defined symbols and user-defined ones.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(*),                  intent(in)  :: predefined_symbols !< Pre-defined compatible symbols.
  character(len=:), allocatable, intent(in)  :: user_symbols       !< User-defined compatible symbols.
  type(string),     allocatable, intent(out) :: symbols_tk(:)      !< Compatible symbols tokenized.
  type(string)                               :: buffer             !< String buffer.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  buffer = predefined_symbols
  if (allocated(user_symbols)) buffer = buffer//','//user_symbols
  call buffer%split(tokens=symbols_tk, sep=',')
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_compatible_symbols_tokens
endmodule fury_unit_abstract
