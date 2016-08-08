!< FURY class definition of real quantity with associated unit of measure.
module fury_qreal
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY class definition of real quantity with associated unit of measure.
!-----------------------------------------------------------------------------------------------------------------------------------
use penf
use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: qreal
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: qreal
  !< Real quantity with associated unit of measure.
  !<
  !< @todo Add units checking.
  private
  real(R_P),    public :: magnitude !< Magnitude of quantity.
  type(string), public :: unit      !< Unit of measure of quantity.
  contains
    ! public methods
    generic :: assignment(=) => assign_qreal !< Overloading `=` assignament.
    generic :: operator(+) => add            !< Overloading `+` operator.
    generic :: operator(/) => div            !< Overloading `/` operator.
    generic :: operator(*) => mul            !< Overloading `*` operator.
    generic :: operator(-) => sub            !< Overloading `-` operator.
    ! private methods
    procedure, pass(lhs), private :: assign_qreal !< `qreal = qreal` assignament.
    procedure, pass(lhs), private :: add          !< `qreal + qreal` operator.
    procedure, pass(lhs), private :: div          !< `qreal / qreal` operator.
    procedure, pass(lhs), private :: mul          !< `qreal * qreal` operator.
    procedure, pass(lhs), private :: sub          !< `qreal - qreal` operator.
endtype qreal
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  elemental subroutine assign_qreal(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal = qreal` assignament.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(inout) :: lhs !< Left hand side.
  type(qreal),  intent(in)    :: rhs !< Right hand side.
  !---------------------------------------------------------------------------------------------------------------------------------
  lhs%magnitude = rhs%magnitude
  lhs%unit = rhs%unit
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_qreal

  elemental function add(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal + qreal` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  type(qreal),  intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------
  opr%magnitude = lhs%magnitude + rhs%magnitude
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction add

  elemental function div(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal / qreal` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  type(qreal),  intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------
  opr%magnitude = lhs%magnitude / rhs%magnitude
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction div

  elemental function mul(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal * qreal` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  type(qreal),  intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------
  opr%magnitude = lhs%magnitude * rhs%magnitude
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction mul

  elemental function sub(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal - qreal` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  type(qreal),  intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------
  opr%magnitude = lhs%magnitude - rhs%magnitude
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction sub
endmodule fury_qreal
