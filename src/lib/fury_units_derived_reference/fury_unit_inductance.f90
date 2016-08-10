!< FURY definition of unit *reference* inductance.
module fury_unit_inductance
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY definition of unit *reference* inductance.
!-----------------------------------------------------------------------------------------------------------------------------------
use fury_unit_abstract
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: unit_inductance
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, extends(unit_abstract) :: unit_inductance
  !< Definition of inductance unit.
  contains
    ! public deferred methods
    procedure, nopass :: is_compatible !< Check if unit is compatible with another one.
endtype unit_inductance

interface unit_inductance
  !< Ovearloading unit_inductance name with a creator function.
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
  type(unit_inductance)              :: unit           !< The unit.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call unit%set(scale_factor=scale_factor, symbol=symbol, dimensionality=dimensionality)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction creator

  ! public deferred methods
  elemental function is_compatible(unit) result(compatible)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if unit is compatible with another one.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(unit_abstract), intent(in) :: unit       !< The other unit.
  logical                          :: compatible !< Compatibility check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  select type(unit)
  class is(unit_inductance)
    compatible = .true.
  class default
    compatible = .false.
  endselect
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_compatible
endmodule fury_unit_inductance
