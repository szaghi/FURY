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
    ! deferred methods
    procedure(conversion),           deferred, nopass    :: convert          !< The conversion formula.
    procedure(assignment_converter), deferred, pass(lhs) :: assign_converter !< `converter = converter` assignment.
    ! operators
    generic :: assignment(=) => assign_converter !< Overloading `=` assignment.
endtype converter

abstract interface
  !< Generic conversion alias formula that must be user-supplied.
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

abstract interface
  !< `converter = converter` assignment.
  pure subroutine assignment_converter(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `converter = converter` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  import converter
  class(converter), intent(inout) :: lhs !< Left hand side.
  class(converter), intent(in)    :: rhs !< Right hand side.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assignment_converter
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
