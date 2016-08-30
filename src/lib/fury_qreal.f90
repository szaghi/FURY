!< FURY class definition of real quantity with associated unit of measure.
module fury_qreal
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY class definition of real quantity with associated unit of measure.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : stderr => error_unit
use fury_uom
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
  real(R_P),               public :: magnitude=0._R_P !< Magnitude of the quantity.
  class(uom), allocatable, public :: unit             !< Unit of measure of the quantity.
  contains
    ! public methods
    procedure, pass(self) :: is_compatible   !< Check if the quantity is compatible with another one.
    procedure, pass(self) :: is_unit_defined !< Check if the unit has been defined.
    procedure, pass(self) :: set             !< Set quantity.
    procedure, pass(self) :: stringify       !< Return a string representaion of the quantity with unit symbol.
    procedure, pass(self) :: to              !< Convert quantity with respect one of its defined unit aliases.
    procedure, pass(self) :: unset           !< Unset quantity.
    ! public generic names
    generic :: assignment(=) => assign_qreal       !< Overloading `=` assignament.
    generic :: operator(+) => add                  !< Overloading `+` operator.
    generic :: operator(/) => div,             &
                              div_R8P, div_R4P,&
                              div_I8P, div_I4P,&
                              div_I2P, div_I1P     !< Overloading `/` operator.
    generic :: operator(*) => mul,             &
                              mul_R8P, mul_R4P,&
                              mul_I8P, mul_I4P,&
                              mul_I2P, mul_I1P,&
                              R8P_mul, R4P_mul,&
                              I8P_mul, I4P_mul,&
                              I2P_mul, I1P_mul     !< Overloading `*` operator.
    generic :: operator(-) => sub                  !< Overloading `-` operator.
    generic :: operator(**) => pow_I8P, pow_I4P, &
                               pow_I2P, pow_I1P    !< Overloading `**` operator.
    ! private methods
    procedure, pass(lhs), private :: assign_qreal !< `qreal = qreal` assignament.
    procedure, pass(lhs), private :: add          !< `qreal + qreal` operator.
    procedure, pass(lhs), private :: div          !< `qreal / qreal` operator.
    procedure, pass(lhs), private :: div_R8P      !< `qreal / real(R8P)` operator.
    procedure, pass(lhs), private :: div_R4P      !< `qreal / real(R4P)` operator.
    procedure, pass(lhs), private :: div_I8P      !< `qreal / integer(I8P)` operator.
    procedure, pass(lhs), private :: div_I4P      !< `qreal / integer(I4P)` operator.
    procedure, pass(lhs), private :: div_I2P      !< `qreal / integer(I2P)` operator.
    procedure, pass(lhs), private :: div_I1P      !< `qreal / integer(I1P)` operator.
    procedure, pass(lhs), private :: mul          !< `qreal * qreal` operator.
    procedure, pass(lhs), private :: mul_R8P      !< `qreal * real(R8P)` operator.
    procedure, pass(lhs), private :: mul_R4P      !< `qreal * real(R4P)` operator.
    procedure, pass(lhs), private :: mul_I8P      !< `qreal * integer(I8P)` operator.
    procedure, pass(lhs), private :: mul_I4P      !< `qreal * integer(I4P)` operator.
    procedure, pass(lhs), private :: mul_I2P      !< `qreal * integer(I2P)` operator.
    procedure, pass(lhs), private :: mul_I1P      !< `qreal * integer(I1P)` operator.
    procedure, pass(rhs), private :: R8P_mul      !< `real(R8P) * qreal` operator.
    procedure, pass(rhs), private :: R4P_mul      !< `real(R4P) * qreal` operator.
    procedure, pass(rhs), private :: I8P_mul      !< `integer(I8P) * qreal` operator.
    procedure, pass(rhs), private :: I4P_mul      !< `integer(I4P) * qreal` operator.
    procedure, pass(rhs), private :: I2P_mul      !< `integer(I2P) * qreal` operator.
    procedure, pass(rhs), private :: I1P_mul      !< `integer(I1P) * qreal` operator.
    procedure, pass(lhs), private :: sub          !< `qreal - qreal` operator.
    procedure, pass(lhs), private :: pow_I8P      !< `qreal ** integer(I8P)` operator.
    procedure, pass(lhs), private :: pow_I4P      !< `qreal ** integer(I4P)` operator.
    procedure, pass(lhs), private :: pow_I2P      !< `qreal ** integer(I2P)` operator.
    procedure, pass(lhs), private :: pow_I1P      !< `qreal ** integer(I1P)` operator.
endtype qreal

interface qreal
  !< Ovearloading [[qreal]] name with a creator function.
  module procedure creator
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! non type bound procedures
  function creator(magnitude, unit) result(quantity)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Create an instance of qreal quantity.
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R_P),  intent(in), optional :: magnitude !< Magnitude of the quantity.
  class(uom), intent(in), optional :: unit      !< Unit of measure of the quantity.
  type(qreal)                      :: quantity  !< The quantity.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call quantity%set(magnitude=magnitude, unit=unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction creator

  subroutine raise_error_incompatibility(operation)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Raise the incompatibility error.
  !---------------------------------------------------------------------------------------------------------------------------------
  character(*), intent(in) :: operation !< Description of the operation errored.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stderr, '(A)')'error: left and right terms of computation "'//operation//'" have incompatible units!'
  write(stderr, '(A)')'result is nullified!'
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine raise_error_incompatibility

  subroutine raise_error_disequality(lhs, rhs, operation)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Raise the incompatibility error.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(qreal),  intent(in) :: lhs       !< Left hand side of the operator.
  type(qreal),  intent(in) :: rhs       !< Rigth hand side of the operator.
  character(*), intent(in) :: operation !< Description of the operation errored.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stderr, '(A)')'  error: left and right terms of "'//operation//'" have disequal units!'
  write(stderr, '(A)')'  LHS: '//lhs%stringify(with_dimensions=.true.)
  write(stderr, '(A)')'  RHS: '//rhs%stringify(with_dimensions=.true.)
  stop
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine raise_error_disequality

  ! public methods
  elemental function is_compatible(self, other) result(compatible)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the quantity is compatible with another one.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: self       !< The quantity.
  type(qreal),  intent(in) :: other      !< The other quantity.
  logical                  :: compatible !< Compatibility check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  compatible = .false.
  if (self%is_unit_defined().and.other%is_unit_defined()) compatible = self%unit.compatible.other%unit
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_compatible

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

  subroutine set(self, magnitude, unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set quantity.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(inout)        :: self      !< The quantity.
  real(R_P),    intent(in), optional :: magnitude !< Magnitude of the quantity.
  class(uom),   intent(in), optional :: unit      !< Unit of measure of the quantity.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(magnitude)) self%magnitude = magnitude
  if (present(unit)) then
    if (allocated(self%unit)) deallocate(self%unit)
    allocate(self%unit, source=unit)
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set

  function stringify(self, format, with_dimensions, with_aliases, compact_reals) result(raw)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a string representaion of the quantity with unit symbol.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in)           :: self            !< The quantity.
  character(*), intent(in), optional :: format          !< Format to pring magnitude.
  logical,      intent(in), optional :: with_dimensions !< Flag to activate dimensions printing.
  logical,      intent(in), optional :: with_aliases    !< Flag to activate aliases printing.
  logical,      intent(in), optional :: compact_reals   !< Flag to activate real numbers compacting.
  character(len=:), allocatable      :: raw             !< Raw characters data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(format)) then
    raw = trim(str(fm=format, n=self%magnitude))
  else
    raw = trim(str(n=self%magnitude, compact=compact_reals))
  endif
  if (self%is_unit_defined()) then
    raw = raw//' '//self%unit%stringify(with_dimensions=with_dimensions, with_aliases=with_aliases, compact_reals=compact_reals)
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction stringify

  function to(self, alias) result(converted)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Convert quantity with respect one of its defined unit aliases.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: self      !< The quantity.
  type(uom),    intent(in) :: alias     !< Unit alias.
  type(qreal)              :: converted !< Quantity converted.
  real(R_P)                :: factor    !< Whole scale factor.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_unit_defined().and.alias%is_defined()) then
    factor = self%unit%get_conversion_factor(alias)
    converted%magnitude = factor * self%magnitude
    allocate(converted%unit)
    converted%unit = alias
  else
    converted = self
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction to

  elemental subroutine unset(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Unset quantity.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(inout) :: self !< The quantity.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%magnitude = 0._R_P
  if (allocated(self%unit)) then
    call self%unit%unset
    deallocate(self%unit)
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine unset

  ! private methods
  subroutine assign_qreal(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal = qreal` assignament.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(inout) :: lhs !< Left hand side.
  type(qreal),  intent(in)    :: rhs !< Right hand side.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (rhs%is_unit_defined()) then
    if (.not.lhs%is_unit_defined())  then
      call lhs%set(magnitude=rhs%magnitude, unit=rhs%unit)
    elseif (lhs%unit == rhs%unit) then
      call lhs%set(magnitude=rhs%magnitude)
    else
      call raise_error_disequality(lhs=lhs, rhs=rhs, operation='LHS = RHS')
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_qreal

  function add(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal + qreal` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  type(qreal),  intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_unit_defined().and.rhs%is_unit_defined()) then
    if (lhs%unit == rhs%unit) then
      opr = lhs
      opr%magnitude = lhs%magnitude + rhs%magnitude
    else
      call raise_error_disequality(lhs=lhs, rhs=rhs, operation='LHS + RHS')
    endif
  else
    ! dimensionless quantities assumed
    opr%magnitude = lhs%magnitude + rhs%magnitude
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction add

  function div(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal / qreal` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  type(qreal),  intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_unit_defined().and.rhs%is_unit_defined()) then
    call opr%set(magnitude=(lhs%magnitude / rhs%magnitude), unit=lhs%unit / rhs%unit)
  else
    ! dimensionless quantities assumed
    opr%magnitude = lhs%magnitude / rhs%magnitude
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction div

  function div_R8P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal / real(R8P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  real(R8P),    intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  opr%magnitude = lhs%magnitude / rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction div_R8P

  function div_R4P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal / real(R4P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  real(R4P),    intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  opr%magnitude = lhs%magnitude / rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction div_R4P

  function div_I8P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal / integer(I8P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  integer(I8P), intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  opr%magnitude = lhs%magnitude / rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction div_I8P

  function div_I4P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal / integer(I4P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  integer(I4P), intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  opr%magnitude = lhs%magnitude / rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction div_I4P

  function div_I2P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal / integer(I2P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  integer(I2P), intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  opr%magnitude = lhs%magnitude / rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction div_I2P

  function div_I1P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal / integer(I1P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  integer(I1P), intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  opr%magnitude = lhs%magnitude / rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction div_I1P

  function mul(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal * qreal` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  type(qreal),  intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_unit_defined().and.rhs%is_unit_defined()) then
    call opr%set(magnitude=(lhs%magnitude * rhs%magnitude), unit=lhs%unit * rhs%unit)
  else
    ! dimensionless quantities assumed
    opr%magnitude = lhs%magnitude * rhs%magnitude
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction mul

  function mul_R8P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal * real(R8P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  real(R8P),    intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  opr%magnitude = lhs%magnitude * rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction mul_R8P

  function mul_R4P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal * real(R4P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  real(R4P),    intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  opr%magnitude = lhs%magnitude * rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction mul_R4P

  function mul_I8P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal * integer(I8P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  integer(I8P), intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  opr%magnitude = lhs%magnitude * rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction mul_I8P

  function mul_I4P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal * integer(I4P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  integer(I4P), intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  opr%magnitude = lhs%magnitude * rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction mul_I4P

  function mul_I2P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal * integer(I2P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  integer(I2P), intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  opr%magnitude = lhs%magnitude * rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction mul_I2P

  function mul_I1P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal * integer(I1P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  integer(I1P), intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  opr%magnitude = lhs%magnitude * rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction mul_I1P

  function R8P_mul(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal * real(R8P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R8P),    intent(in) :: lhs !< Left hand side.
  class(qreal), intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = rhs
  opr%magnitude = lhs * rhs%magnitude
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction R8P_mul

  function R4P_mul(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal * real(R4P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R4P),    intent(in) :: lhs !< Left hand side.
  class(qreal), intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = rhs
  opr%magnitude = lhs * rhs%magnitude
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction R4P_mul

  function I8P_mul(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal * integer(I8P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I8P), intent(in) :: lhs !< Left hand side.
  class(qreal), intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = rhs
  opr%magnitude = lhs * rhs%magnitude
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction I8P_mul

  function I4P_mul(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal * integer(I4P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(in) :: lhs !< Left hand side.
  class(qreal), intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = rhs
  opr%magnitude = lhs * rhs%magnitude
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction I4P_mul

  function I2P_mul(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal * integer(I2P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I2P), intent(in) :: lhs !< Left hand side.
  class(qreal), intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = rhs
  opr%magnitude = lhs * rhs%magnitude
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction I2P_mul

  function I1P_mul(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal * integer(I1P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I1P), intent(in) :: lhs !< Left hand side.
  class(qreal), intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = rhs
  opr%magnitude = lhs * rhs%magnitude
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction I1P_mul

  function sub(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal - qreal` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  type(qreal),  intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_unit_defined().and.rhs%is_unit_defined()) then
    if (lhs%unit == rhs%unit) then
      opr = lhs
      opr%magnitude = lhs%magnitude - rhs%magnitude
    else
      call raise_error_disequality(lhs=lhs, rhs=rhs, operation='LHS + RHS')
    endif
  else
    ! dimensionless quantities assumed
    opr%magnitude = lhs%magnitude - rhs%magnitude
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction sub

  function pow_I8P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal ** integer(I8P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  integer(I8P), intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr%magnitude = lhs%magnitude ** rhs
  if (lhs%is_unit_defined()) then
    allocate(opr%unit)
    opr%unit = lhs%unit ** rhs
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I8P

  function pow_I4P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal ** integer(I4P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  integer(I4P), intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr%magnitude = lhs%magnitude ** rhs
  if (lhs%is_unit_defined()) then
    allocate(opr%unit)
    opr%unit = lhs%unit ** rhs
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I4P

  function pow_I2P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal ** integer(I2P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  integer(I2P), intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr%magnitude = lhs%magnitude ** rhs
  if (lhs%is_unit_defined()) then
    allocate(opr%unit)
    opr%unit = lhs%unit ** rhs
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I2P

  function pow_I1P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal ** integer(I1P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  integer(I1P), intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr%magnitude = lhs%magnitude ** rhs
  if (lhs%is_unit_defined()) then
    allocate(opr%unit)
    opr%unit = lhs%unit ** rhs
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I1P
endmodule fury_qreal
