!< FURY class definition of real quantity with associated unit of measure.
module fury_qreal
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY class definition of real quantity with associated unit of measure.
!-----------------------------------------------------------------------------------------------------------------------------------
use fury_unit_abstract
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
  real(R_P),                         public :: magnitude=0._R_P !< Magnitude of quantity.
  class(unit_abstract), allocatable, public :: unit             !< Unit of measure of quantity.
  contains
    ! public methods
    procedure, pass(self) :: is_unit_defined !< Check if the unit has been defined.
    procedure, pass(self) :: set             !< Set quantity magnitude/unit.
    ! public generic names
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

interface qreal
  !< Ovearloading qreal name with a creator function.
  module procedure creator
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! non type bound procedures
  elemental function creator(magnitude, unit) result(quantity)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Create an instance of qreal quantity.
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R_P),            intent(in), optional :: magnitude !< Magnitude of quantity.
  class(unit_abstract), intent(in), optional :: unit      !< Unit of measure of quantity.
  type(qreal)                                :: quantity  !< The quantity.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call quantity%set(magnitude=magnitude, unit=unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction creator

  ! public methods
  elemental subroutine set(self, magnitude, unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set quantity magnitude/unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal),         intent(inout)        :: self      !< The quantity.
  real(R_P),            intent(in), optional :: magnitude !< Magnitude of quantity.
  class(unit_abstract), intent(in), optional :: unit      !< Unit of measure of quantity.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(magnitude)) self%magnitude = magnitude
  if (present(unit)) then
    if (allocated(self%unit)) deallocate(self%unit)
    allocate(self%unit, source=unit)
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set

  elemental function is_unit_defined(self) result(is_defined)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the unit has been defined.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: self       !< The quantity.
  logical                  :: is_defined !< Unit definition status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_defined = allocated(self%unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_unit_defined

  ! public methods
  elemental subroutine assign_qreal(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal = qreal` assignament.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(inout) :: lhs !< Left hand side.
  type(qreal),  intent(in)    :: rhs !< Right hand side.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_unit_defined().and.rhs%is_unit_defined()) then
    if (lhs%unit%is_compatible(rhs%unit)) then
      lhs%magnitude = rhs%magnitude
    else
      ! raise a sort of error...
    endif
  else
    ! dimensionless quantities assumed
    lhs%magnitude = rhs%magnitude
  endif
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

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_unit_defined().and.rhs%is_unit_defined()) then
    if (lhs%unit%is_compatible(rhs%unit)) then
      opr%magnitude = lhs%magnitude + rhs%magnitude
    else
      ! raise a sort of error...
    endif
  else
    ! dimensionless quantities assumed
    opr%magnitude = lhs%magnitude + rhs%magnitude
  endif
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

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_unit_defined().and.rhs%is_unit_defined()) then
    if (lhs%unit%is_compatible(rhs%unit)) then
      opr%magnitude = lhs%magnitude - rhs%magnitude
    else
      ! raise a sort of error...
    endif
  else
    ! dimensionless quantities assumed
    opr%magnitude = lhs%magnitude - rhs%magnitude
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction sub
endmodule fury_qreal
