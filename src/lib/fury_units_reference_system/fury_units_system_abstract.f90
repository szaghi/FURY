!< FURY definition of abstract units system class.
module fury_units_system_abstract
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY definition of abstract units system class.
!-----------------------------------------------------------------------------------------------------------------------------------
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: units_system_abstract
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, abstract :: units_system_abstract
  !< Abstract prototype of *units system*.
  character(len=:), allocatable :: acronym !< Units system acronym, e.g. "SI" for the International System.
  contains
    ! public deferred methods
    procedure(initialize_interface), pass(self), deferred :: initialize !< Initialize the units system.
endtype units_system_abstract

abstract interface
  !< Check if unit is compatible with .
  subroutine initialize_interface(self, acronym, error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Initialize the units system.
  !---------------------------------------------------------------------------------------------------------------------------------
  import I_P, units_system_abstract
  class(units_system_abstract), intent(inout)         :: self    !< The units system.
  character(*),                 intent(in),  optional :: acronym !< Units system acronym, e.g. "SI" for the International System.
  integer(I_P),                 intent(out), optional :: error   !< Error code, 0 => no errors happen.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine initialize_interface
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
endmodule fury_units_system_abstract
