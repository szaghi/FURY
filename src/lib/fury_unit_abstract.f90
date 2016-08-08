!< FURY definition of abstract unit class.
module fury_unit_abstract
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY definition of abstract unit class.
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
logical                   :: initialized=.false. !< Flag to check is units definitions are initialized.
type(string), allocatable :: metre(:)            !< Metre symbols.

type, abstract :: unit_abstract
  real(R_P)    :: scale_factor !< Scale factor for multiple of base unit, e.g. 1000 for kilometres.
  type(string) :: symbol       !< Litteral symbol(s) of the unit, e.g. "m" for metres.
  contains
    procedure(initialize_interface),    pass(self), deferred :: initialize    !< Initialize the unit.
    procedure(is_compatible_interface), pass(self), deferred :: is_compatible !< Check if unit is compatible with another.
endtype unit_abstract

abstract interface
  !< Initialize the unit.
  subroutine initialize_interface(self, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Initialize units definition.
  !---------------------------------------------------------------------------------------------------------------------------------
  import I_P, unit_abstract
  class(unit_abstract), intent(inout)         :: self  !< The unit.
  integer(I_P),         intent(out), optional :: error !< Initializing error code, 0 => no errors happen.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine initialize_interface
endinterface

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
!-----------------------------------------------------------------------------------------------------------------------------------
endmodule fury_unit_abstract
