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
! use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: unit_abstract
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, abstract :: unit_abstract
  !< Abstract prototype of *unit*.
  real(R_P)                     :: scale_factor   !< Scale factor for multiple of base unit, e.g. 1000 for kilometres.
  character(len=:), allocatable :: symbol         !< Litteral symbol(s) of the unit, e.g. "m" for metres.
  character(len=:), allocatable :: dimensionality !< Reference dimensionality symbol, e.g. "[length]" for metres.
  contains
    ! public deferred methods
    procedure(is_compatible_interface), nopass, deferred :: is_compatible !< Check if unit is compatible with another one.
    ! public methods
    procedure, pass(self) :: set !< Set the unit.
endtype unit_abstract

abstract interface
  !< Check if unit is compatible with .
  elemental function is_compatible_interface(unit) result(compatible)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if unit is compatible with another one.
  !---------------------------------------------------------------------------------------------------------------------------------
  import unit_abstract
  class(unit_abstract), intent(in) :: unit       !< The other unit.
  logical                          :: compatible !< Compatibility check result.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_compatible_interface
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! public methods
  subroutine set(self, scale_factor, symbol, dimensionality, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set the unit.
  !<
  !< @todo Load from file.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_abstract), intent(inout)         :: self           !< The unit.
  real(R_P),            intent(in),  optional :: scale_factor   !< Scale factor for multiple of base unit, e.g. 1000 for kilometres.
  character(*),         intent(in),  optional :: symbol         !< Litteral symbol of the unit, e.g. "m" for metres.
  character(*),         intent(in),  optional :: dimensionality !< Reference dimensionality symbol, e.g. "[length]" for metres.
  integer(I_P),         intent(out), optional :: error          !< Error code, 0 => no errors happen.
  integer(I_P)                                :: error_         !< Error code, 0 => no errors happen, local variable.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  error_ = 1
  if (present(scale_factor)) self%scale_factor = scale_factor
  if (present(symbol)) self%symbol = symbol
  if (present(dimensionality)) self%dimensionality = dimensionality
  error_ = 0
  if (present(error)) error = error_
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set
endmodule fury_unit_abstract
