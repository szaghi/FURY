!< FURY class definition of unit converter.
module fury_uom_converter
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY class definition of unit converter.
!-----------------------------------------------------------------------------------------------------------------------------------
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: uom_converter
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, abstract :: uom_converter
  !< A generic user-supplied [[uom_symbol]] converter.
  contains
    ! deferred methods
#ifdef r16p
    procedure(conversion_float128),  deferred, nopass    :: convert_float128 !< The conversion formulas for float128 magnitudes.
#endif
    procedure(conversion_float64),   deferred, nopass    :: convert_float64  !< The conversion formulas for float64 magnitudes.
    procedure(conversion_float32),   deferred, nopass    :: convert_float32  !< The conversion formulas for float32 magnitudes.
    procedure(assignment_converter), deferred, pass(lhs) :: assign_converter !< `converter = converter` assignment.
    ! generic methods
    generic :: convert =>        &
#ifdef r16p
               convert_float128, &
#endif
               convert_float64, convert_float32 !< The conversion formulas.
    ! operators
    generic :: assignment(=) => assign_converter !< Overloading `=` assignment.
endtype uom_converter

abstract interface
  !< Generic conversion alias formula that must be user-supplied, float128.
  pure function conversion_float128(magnitude, inverse) result(converted)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Generic conversion alias formula that must be user-supplied, float128.
  !---------------------------------------------------------------------------------------------------------------------------------
  import R16P
  real(R16P), intent(in)           :: magnitude !< Magnitude (of the quantity) to be converted.
  logical,    intent(in), optional :: inverse   !< Activate inverse conversion.
  real(R16P)                       :: converted !< Converted magnitude.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction conversion_float128
endinterface

abstract interface
  !< Generic conversion alias formula that must be user-supplied, float64.
  pure function conversion_float64(magnitude, inverse) result(converted)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Generic conversion alias formula that must be user-supplied, float64.
  !---------------------------------------------------------------------------------------------------------------------------------
  import R8P
  real(R8P), intent(in)           :: magnitude !< Magnitude (of the quantity) to be converted.
  logical,   intent(in), optional :: inverse   !< Activate inverse conversion.
  real(R8P)                       :: converted !< Converted magnitude.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction conversion_float64
endinterface

abstract interface
  !< Generic conversion alias formula that must be user-supplied, float32.
  pure function conversion_float32(magnitude, inverse) result(converted)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Generic conversion alias formula that must be user-supplied, float32.
  !---------------------------------------------------------------------------------------------------------------------------------
  import R4P
  real(R4P), intent(in)           :: magnitude !< Magnitude (of the quantity) to be converted.
  logical,   intent(in), optional :: inverse   !< Activate inverse conversion.
  real(R4P)                       :: converted !< Converted magnitude.
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction conversion_float32
endinterface

abstract interface
  !< `converter = converter` assignment.
  pure subroutine assignment_converter(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `converter = converter` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  import uom_converter
  class(uom_converter), intent(inout) :: lhs !< Left hand side.
  class(uom_converter), intent(in)    :: rhs !< Right hand side.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assignment_converter
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
endmodule fury_uom_converter
