!< FURY class definition of unit converter with generic kind.
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY class definition of unit converter with generic kind.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: converter
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, abstract :: converter
  !< A generic user-supplied [[uom_symbol]] converter.
  contains
    procedure(conversion), deferred, nopass :: convert !< The conversion formula.
endtype converter

abstract interface
  pure function conversion(magnitude, inverse) result(converted)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Generic conversion alias formula that must be user-supplied.
  !---------------------------------------------------------------------------------------------------------------------------------
  import RKP
  real(RKP), intent(in)           :: magnitude !< Magnitude (of the quantity) to be converted.
  logical,   intent(in), optional :: inverse   !< Activate inverse conversion.
  real(RKP)                       :: converted !< Converted magnitude.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction conversion
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
