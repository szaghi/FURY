!< FURY definition of abstract units system class.
module fury_units_system_abstract
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY definition of abstract units system class.
!-----------------------------------------------------------------------------------------------------------------------------------
use fury_unit_abstract
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
    procedure(associate_unit_interface), pass(self), deferred :: associate_unit !< Associate unit by dimensionality.
    procedure(initialize_interface),     pass(self), deferred :: initialize     !< Initialize the units system.
endtype units_system_abstract

abstract interface
  !< Associate unit by dimensionality.
  subroutine associate_unit_interface(self, dimensionality, unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Associate unit by dimensionality.
  !---------------------------------------------------------------------------------------------------------------------------------
  import unit_abstract, units_system_abstract
  class(units_system_abstract), intent(in), target   :: self           !< The units system.
  character(*),                 intent(in)           :: dimensionality !< Reference dimensionality symbol, e.g. "[length]" for m.
  class(unit_abstract),         intent(out), pointer :: unit=>null()   !< Unit of measure of quantity.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine associate_unit_interface
endinterface

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
