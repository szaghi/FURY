!< FURY class definition of real quantity with associated unit of measure.
module fury_qreal
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY class definition of real quantity with associated unit of measure.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : stderr => error_unit
use fury_unit_generic
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
  real(R_P),                        public :: magnitude=0._R_P !< Magnitude of the quantity.
  class(unit_generic), allocatable, public :: unit             !< Unit of measure of the quantity.
  contains
    ! public methods
    procedure, pass(self) :: is_compatible   !< Check if the quantity is compatible with another one.
    procedure, pass(self) :: is_unit_defined !< Check if the unit has been defined.
    procedure, pass(self) :: set             !< Set quantity.
    procedure, pass(self) :: stringify       !< Return a string representaion of the quantity with unit symbol.
    procedure, pass(self) :: unset           !< Unset quantity.
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
  real(R_P),           intent(in), optional :: magnitude !< Magnitude of the quantity.
  class(unit_generic), intent(in), optional :: unit      !< Unit of measure of the quantity.
  type(qreal)                               :: quantity  !< The quantity.
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
  if (self%is_unit_defined().and.other%is_unit_defined()) compatible = self%unit%is_compatible(other%unit)
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
  class(qreal),        intent(inout)        :: self      !< The quantity.
  real(R_P),           intent(in), optional :: magnitude !< Magnitude of the quantity.
  class(unit_generic), intent(in), optional :: unit      !< Unit of measure of the quantity.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(magnitude)) self%magnitude = magnitude
  if (present(unit)) then
    if (allocated(self%unit)) deallocate(self%unit)
    allocate(self%unit, source=unit)
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set

  pure function stringify(self, format, with_dimensions) result(raw)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a string representaion of the quantity with unit symbol.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in)           :: self            !< The quantity.
  character(*), intent(in), optional :: format          !< Format to pring magnitude.
  logical,      intent(in), optional :: with_dimensions !< Flag to activate dimensions printing.
  character(len=:), allocatable      :: raw             !< Raw characters data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(format)) then
    raw = trim(str(fm=format, n=self%magnitude))
  else
    raw = trim(str(n=self%magnitude))
  endif
  if (self%is_unit_defined()) then
    raw = raw//self%unit%stringify(with_dimensions=with_dimensions)
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction stringify

  elemental subroutine unset(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Unset quantity.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(inout) :: self !< The quantity.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%magnitude = 0._R_P
  if (allocated(self%unit)) deallocate(self%unit)
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
  call lhs%set(magnitude=rhs%magnitude, unit=rhs%unit)
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
    if (lhs%is_compatible(other=rhs)) then
      call opr%set(magnitude=(lhs%magnitude + rhs%magnitude), unit=lhs%unit)
    else
      call raise_error_incompatibility(operation='L+R')
      call opr%unset
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
    if (lhs%unit%is_compatible(other=rhs%unit)) then
      call opr%set(magnitude=(lhs%magnitude - rhs%magnitude), unit=lhs%unit)
    else
      call raise_error_incompatibility(operation='L-R')
      call opr%unset
    endif
  else
    ! dimensionless quantities assumed
    opr%magnitude = lhs%magnitude - rhs%magnitude
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction sub
endmodule fury_qreal
