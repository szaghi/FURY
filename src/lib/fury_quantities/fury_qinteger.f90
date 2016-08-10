!< FURY class definition of integer quantity with associated unit of measure.
module fury_qinteger
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY class definition of integer quantity with associated unit of measure.
!<
!< @todo Work only on `qreal`, `qinteger` class will be updated soon or late...
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
use fury_unit_abstract
use penf
use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: qinteger
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: qinteger
  !< Integer quantity with associated unit of measure.
  !<
  !< @todo Add units checking.
  private
  integer(I_P),                      public :: magnitude !< Magnitude of quantity.
  class(unit_abstract), allocatable, public :: unit      !< Unit of measure of quantity.
  contains
    ! public methods
    generic :: assignment(=) => assign_qinteger !< Overloading `=` assignament.
    generic :: operator(+) => add               !< Overloading `+` operator.
    generic :: operator(/) => div               !< Overloading `/` operator.
    generic :: operator(*) => mul               !< Overloading `*` operator.
    generic :: operator(-) => sub               !< Overloading `-` operator.
    ! private methods
    procedure, pass(lhs), private :: assign_qinteger !< `qinteger = qinteger` assignament.
    procedure, pass(lhs), private :: add             !< `qinteger + qinteger` operator.
    procedure, pass(lhs), private :: div             !< `qinteger / qinteger` operator.
    procedure, pass(lhs), private :: mul             !< `qinteger * qinteger` operator.
    procedure, pass(lhs), private :: sub             !< `qinteger - qinteger` operator.
endtype qinteger
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  elemental subroutine assign_qinteger(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qinteger = qinteger` assignament.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qinteger), intent(inout) :: lhs !< Left hand side.
  type(qinteger),  intent(in)    :: rhs !< Right hand side.
  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%unit%is_compatible(rhs%unit)) lhs%magnitude = rhs%magnitude
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_qinteger

  elemental function add(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qinteger + qinteger` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qinteger), intent(in) :: lhs !< Left hand side.
  type(qinteger),  intent(in) :: rhs !< Right hand side.
  type(qinteger)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%unit%is_compatible(rhs%unit)) opr%magnitude = lhs%magnitude + rhs%magnitude
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction add

  elemental function div(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qinteger / qinteger` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qinteger), intent(in) :: lhs !< Left hand side.
  type(qinteger),  intent(in) :: rhs !< Right hand side.
  type(qinteger)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------
  opr%magnitude = lhs%magnitude / rhs%magnitude
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction div

  elemental function mul(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qinteger * qinteger` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qinteger), intent(in) :: lhs !< Left hand side.
  type(qinteger),  intent(in) :: rhs !< Right hand side.
  type(qinteger)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------
  opr%magnitude = lhs%magnitude * rhs%magnitude
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction mul

  elemental function sub(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qinteger - qinteger` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qinteger), intent(in) :: lhs !< Left hand side.
  type(qinteger),  intent(in) :: rhs !< Right hand side.
  type(qinteger)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%unit%is_compatible(rhs%unit)) opr%magnitude = lhs%magnitude - rhs%magnitude
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction sub
endmodule fury_qinteger
