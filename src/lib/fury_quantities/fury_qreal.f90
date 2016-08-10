!< FURY class definition of real quantity with associated unit of measure.
module fury_qreal
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY class definition of real quantity with associated unit of measure.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : stderr => error_unit
use fury_unit_abstract
use fury_unit_unknown
use fury_units_system_abstract
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
  !<
  !< @note The result of the operator `+` and `-` between two `qreal` has the unit of the left term, namely `q` has the unit
  !< of `q1` in the sum `q=q1+q2`. Note also that the operation is done in the *norm* of scaling factor of the left term, namely
  !< `q=q1+ scale2*q2/scale1` is the operation made if `q1` and `q2` have different scale factors, e.g. if `q1` is in metre and
  !< `q2` is in kilometre the actual operation is `q=q1*1000*q2`: take care about this, possible losts of accuracy are likely.
  real(R_P),                             public :: magnitude=0._R_P     !< Magnitude of quantity.
  class(unit_abstract),         pointer, public :: unit=>null()         !< Unit of measure of quantity.
  class(units_system_abstract), pointer, public :: units_system=>null() !< Units system.
  contains
    ! public methods
    procedure, pass(self) :: is_unit_defined         !< Check if the unit has been defined.
    procedure, pass(self) :: is_units_system_defined !< Check if the units system has been defined.
    procedure, pass(self) :: set                     !< Set quantity magnitude/unit/units system.
    procedure, pass(self) :: stringify               !< Return a string representaion of the quantity with unit symbol.
    procedure, pass(self) :: unset                   !< Unset quantity.
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
  function creator(magnitude, unit, units_system) result(quantity)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Create an instance of qreal quantity.
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R_P),                    intent(in),         optional :: magnitude    !< Magnitude of quantity.
  class(unit_abstract),         intent(in), target, optional :: unit         !< Unit of measure of quantity.
  class(units_system_abstract), intent(in), target, optional :: units_system !< Units system.
  type(qreal)                                                :: quantity     !< The quantity.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call quantity%set(magnitude=magnitude, unit=unit, units_system=units_system)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction creator

  ! public methods
  elemental function is_unit_defined(self) result(is_defined)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the unit has been defined.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: self       !< The quantity.
  logical                  :: is_defined !< Unit definition status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_defined = associated(self%unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_unit_defined

  elemental function is_units_system_defined(self) result(is_defined)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the units system has been defined.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: self       !< The quantity.
  logical                  :: is_defined !< Units system definition status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_defined = associated(self%units_system)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_units_system_defined

  subroutine set(self, magnitude, unit, units_system)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set quantity magnitude/unit/units system.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal),                 intent(inout)                :: self         !< The quantity.
  real(R_P),                    intent(in),         optional :: magnitude    !< Magnitude of quantity.
  class(unit_abstract),         intent(in), target, optional :: unit         !< Unit of measure of quantity.
  class(units_system_abstract), intent(in), target, optional :: units_system !< Units system.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(magnitude)) self%magnitude = magnitude
  if (present(unit)) self%unit => unit
  if (present(units_system)) self%units_system => units_system
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set

  pure function stringify(self, format) result(raw)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a string representaion of the quantity with unit symbol.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in)           :: self   !< The quantity.
  character(*), intent(in), optional :: format !< Format to pring magnitude.
  character(len=:), allocatable      :: raw    !< Raw characters data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(format)) then
    raw = trim(str(fm=format, n=self%magnitude))
  else
    raw = trim(str(n=self%magnitude))
  endif
  if (self%is_unit_defined()) raw = raw//self%unit%symbol
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
  self%unit => null()
  self%units_system => null()
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
  call lhs%set(magnitude=rhs%magnitude, unit=rhs%unit, units_system=rhs%units_system)
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
    if (lhs%unit%is_compatible(rhs%unit)) then
      call opr%set(magnitude=(lhs%magnitude + rhs%unit%scale_factor*rhs%magnitude/lhs%unit%scale_factor), &
                   unit=lhs%unit, units_system=lhs%units_system)
    else
      write(stderr, '(A)')'error: left and right terms of computation "l+r" have incompatible units!'
      write(stderr, '(A)')'result is nullified with units set equal to the one of left term!'
      call opr%set(unit=lhs%unit, units_system=lhs%units_system)
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
  class(qreal), intent(in) :: lhs      !< Left hand side.
  type(qreal),  intent(in) :: rhs      !< Right hand side.
  type(qreal)              :: opr      !< Operator result.
  type(unit_unknown)       :: new_unit !< New type of unit not already defined into the units system used.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_unit_defined().and.rhs%is_unit_defined().and.lhs%is_units_system_defined().and.rhs%is_units_system_defined()) then
    ! here should come the logics for different units systems scenario
    call opr%set(magnitude=(lhs%magnitude / rhs%magnitude), units_system=lhs%units_system)
    call lhs%units_system%associate_unit(dimensionality=lhs%unit%dimensionality//'/'//rhs%unit%dimensionality, unit=opr%unit)
    if (.not.associated(opr%unit)) then
      write(stderr, '(A)')'error: left and right terms of computation "l/r" produce an unknown unit!'
      write(stderr, '(A)')'a new unknown unit is associated to the result''s unit!'
      new_unit = unit_unknown(scale_factor=1._R_P,                          &
                              symbol=lhs%unit%symbol//'/'//rhs%unit%symbol, &
                              dimensionality=lhs%unit%dimensionality//'/'//rhs%unit%dimensionality)
      allocate(opr%unit, source=new_unit)
    endif
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
  class(qreal), intent(in) :: lhs      !< Left hand side.
  type(qreal),  intent(in) :: rhs      !< Right hand side.
  type(qreal)              :: opr      !< Operator result.
  type(unit_unknown)       :: new_unit !< New type of unit not already defined into the units system used.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_unit_defined().and.rhs%is_unit_defined().and.lhs%is_units_system_defined().and.rhs%is_units_system_defined()) then
    ! here should come the logics for different units systems scenario
    call opr%set(magnitude=(lhs%magnitude * rhs%magnitude), units_system=lhs%units_system)
    call lhs%units_system%associate_unit(dimensionality=lhs%unit%dimensionality//'*'//rhs%unit%dimensionality, unit=opr%unit)
    if (.not.associated(opr%unit)) then
      write(stderr, '(A)')'error: left and right terms of computation "l*r" produce an unknown unit!'
      write(stderr, '(A)')'a new unknown unit is associated to the result''s unit!'
      new_unit = unit_unknown(scale_factor=1._R_P,                          &
                              symbol=lhs%unit%symbol//'*'//rhs%unit%symbol, &
                              dimensionality=lhs%unit%dimensionality//'*'//rhs%unit%dimensionality)
      allocate(opr%unit, source=new_unit)
    endif
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
    if (lhs%unit%is_compatible(rhs%unit)) then
      call opr%set(magnitude=(lhs%magnitude - rhs%unit%scale_factor*rhs%magnitude/lhs%unit%scale_factor), &
                   unit=lhs%unit, units_system=lhs%units_system)
    else
      write(stderr, '(A)')'error: left and right terms of computation "l-r" have incompatible units!'
      write(stderr, '(A)')'result is nullified with units set equal to the one of left term!'
      call opr%set(unit=lhs%unit, units_system=lhs%units_system)
    endif
  else
    ! dimensionless quantities assumed
    opr%magnitude = lhs%magnitude - rhs%magnitude
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction sub
endmodule fury_qreal
