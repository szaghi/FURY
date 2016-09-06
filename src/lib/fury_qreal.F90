!< FURY class definition of real quantity with associated unit of measure.
module fury_qreal
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY class definition of real quantity with associated unit of measure.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : stderr => error_unit
use fury_uom
use penf, RKP => R_P
use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: operator(*)
public :: qreal
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: qreal
  !< Real quantity with associated unit of measure.
  real(RKP),                     public :: magnitude=0._RKP !< Magnitude of the quantity.
  class(uom),       allocatable, public :: unit             !< Unit of measure of the quantity.
  character(len=:), allocatable, public :: name             !< Quantity name.
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
    generic :: operator(+) => add, positive        !< Overloading `+` operator.
    generic :: operator(/) => div,              &
#ifdef r16p
                              div_R16P,         &
#endif
                              div_R8P, div_R4P, &
                              div_I8P, div_I4P, &
                              div_I2P, div_I1P     !< Overloading `/` operator.
    generic :: operator(*) => mul,              &
#ifdef r16p
                              mul_R16P,         &
#endif
                              mul_R8P, mul_R4P, &
                              mul_I8P, mul_I4P, &
                              mul_I2P, mul_I1P, &
#ifdef r16p
                              R16P_mul,         &
#endif
                              R8P_mul, R4P_mul, &
                              I8P_mul, I4P_mul, &
                              I2P_mul, I1P_mul     !< Overloading `*` operator.
    generic :: operator(-) => sub, negative        !< Overloading `-` operator.
    generic :: operator(**) =>                   &
#ifdef r16p
                               pow_R16P,         &
#endif
                               pow_R8P, pow_R4P, &
                               pow_I8P, pow_I4P, &
                               pow_I2P, pow_I1P    !< Overloading `**` operator.
    generic :: operator(==) => is_equal            !< Overloading `==` operator.
    generic :: operator(/=) => is_not_equal        !< Overloading `/=` operator.
    ! private methods
    procedure, pass(self), private :: is_equal     !< Check if qreal is equal with another one.
    procedure, pass(self), private :: is_not_equal !< Check if qreal is not equal with another one.
    procedure, pass(lhs),  private :: assign_qreal !< `qreal = qreal` assignament.
    procedure, pass(lhs),  private :: add          !< `qreal + qreal` operator.
    procedure, pass(self), private :: positive     !< ` + qreal` unary operator.
    procedure, pass(lhs),  private :: div          !< `qreal / qreal` operator.
    procedure, pass(lhs),  private :: div_R16P     !< `qreal / real(R16P)` operator.
    procedure, pass(lhs),  private :: div_R8P      !< `qreal / real(R8P)` operator.
    procedure, pass(lhs),  private :: div_R4P      !< `qreal / real(R4P)` operator.
    procedure, pass(lhs),  private :: div_I8P      !< `qreal / integer(I8P)` operator.
    procedure, pass(lhs),  private :: div_I4P      !< `qreal / integer(I4P)` operator.
    procedure, pass(lhs),  private :: div_I2P      !< `qreal / integer(I2P)` operator.
    procedure, pass(lhs),  private :: div_I1P      !< `qreal / integer(I1P)` operator.
    procedure, pass(lhs),  private :: mul          !< `qreal * qreal` operator.
    procedure, pass(lhs),  private :: mul_R16P     !< `qreal * real(R16P)` operator.
    procedure, pass(lhs),  private :: mul_R8P      !< `qreal * real(R8P)` operator.
    procedure, pass(lhs),  private :: mul_R4P      !< `qreal * real(R4P)` operator.
    procedure, pass(lhs),  private :: mul_I8P      !< `qreal * integer(I8P)` operator.
    procedure, pass(lhs),  private :: mul_I4P      !< `qreal * integer(I4P)` operator.
    procedure, pass(lhs),  private :: mul_I2P      !< `qreal * integer(I2P)` operator.
    procedure, pass(lhs),  private :: mul_I1P      !< `qreal * integer(I1P)` operator.
    procedure, pass(rhs),  private :: R16P_mul     !< `real(R16P) * qreal` operator.
    procedure, pass(rhs),  private :: R8P_mul      !< `real(R8P) * qreal` operator.
    procedure, pass(rhs),  private :: R4P_mul      !< `real(R4P) * qreal` operator.
    procedure, pass(rhs),  private :: I8P_mul      !< `integer(I8P) * qreal` operator.
    procedure, pass(rhs),  private :: I4P_mul      !< `integer(I4P) * qreal` operator.
    procedure, pass(rhs),  private :: I2P_mul      !< `integer(I2P) * qreal` operator.
    procedure, pass(rhs),  private :: I1P_mul      !< `integer(I1P) * qreal` operator.
    procedure, pass(lhs),  private :: sub          !< `qreal - qreal` operator.
    procedure, pass(self), private :: negative     !< ` - qreal` unary operator.
    procedure, pass(lhs),  private :: pow_R16P     !< `qreal ** real(R16P)` operator.
    procedure, pass(lhs),  private :: pow_R8P      !< `qreal ** real(R8P)` operator.
    procedure, pass(lhs),  private :: pow_R4P      !< `qreal ** real(R4P)` operator.
    procedure, pass(lhs),  private :: pow_I8P      !< `qreal ** integer(I8P)` operator.
    procedure, pass(lhs),  private :: pow_I4P      !< `qreal ** integer(I4P)` operator.
    procedure, pass(lhs),  private :: pow_I2P      !< `qreal ** integer(I2P)` operator.
    procedure, pass(lhs),  private :: pow_I1P      !< `qreal ** integer(I1P)` operator.
endtype qreal

interface qreal
  !< Ovearloading [[qreal]] name with a creator function.
  module procedure creator
endinterface

interface operator(*)
  !< Ovearloading operator `*`: `number *` [[uom]] generates [[qreal]].
  module procedure              &
#ifdef r16p
      R16P_mul_uom,             &
#endif
      R8P_mul_uom, R4P_mul_uom, &
      I8P_mul_uom, I4P_mul_uom, I2P_mul_uom, I1P_mul_uom
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! private non type bound procedures
  function creator(magnitude, unit, name) result(quantity)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Create an instance of qreal quantity.
  !---------------------------------------------------------------------------------------------------------------------------------
  real(RKP),    intent(in), optional :: magnitude !< Magnitude of the quantity.
  class(uom),   intent(in), optional :: unit      !< Unit of measure of the quantity.
  character(*), intent(in), optional :: name      !< Quantity name.
  type(qreal)                        :: quantity  !< The quantity.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call quantity%set(magnitude=magnitude, unit=unit, name=name)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction creator

  function R16P_mul_uom(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `real(R16P) * uom` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R16P), intent(in) :: lhs !< Left hand side.
  type(uom),  intent(in) :: rhs !< Right hand side.
  type(qreal)            :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = qreal(magnitude=real(lhs, kind=RKP), unit=rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction R16P_mul_uom

  function R8P_mul_uom(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `real(R8P) * uom` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R8P), intent(in) :: lhs !< Left hand side.
  type(uom), intent(in) :: rhs !< Right hand side.
  type(qreal)           :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = qreal(magnitude=real(lhs, kind=RKP), unit=rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction R8P_mul_uom

  function R4P_mul_uom(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `real(R4P) * uom` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R4P), intent(in) :: lhs !< Left hand side.
  type(uom), intent(in) :: rhs !< Right hand side.
  type(qreal)           :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = qreal(magnitude=real(lhs, kind=RKP), unit=rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction R4P_mul_uom

  function I8P_mul_uom(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `integer(I8P) * uom` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I8P), intent(in) :: lhs !< Left hand side.
  type(uom),    intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = qreal(magnitude=real(lhs, kind=RKP), unit=rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction I8P_mul_uom

  function I4P_mul_uom(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `integer(I4P) * uom` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I4P), intent(in) :: lhs !< Left hand side.
  type(uom),    intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = qreal(magnitude=real(lhs, kind=RKP), unit=rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction I4P_mul_uom

  function I2P_mul_uom(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `integer(I2P) * uom` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I2P), intent(in) :: lhs !< Left hand side.
  type(uom),    intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = qreal(magnitude=real(lhs, kind=RKP), unit=rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction I2P_mul_uom

  function I1P_mul_uom(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `integer(I1P) * uom` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  integer(I1P), intent(in) :: lhs !< Left hand side.
  type(uom),    intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = qreal(magnitude=real(lhs, kind=RKP), unit=rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction I1P_mul_uom

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

  subroutine set(self, magnitude, unit, name)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set quantity.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(inout)        :: self      !< The quantity.
  real(RKP),    intent(in), optional :: magnitude !< Magnitude of the quantity.
  class(uom),   intent(in), optional :: unit      !< Unit of measure of the quantity.
  character(*), intent(in), optional :: name      !< Quantity name.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(magnitude)) self%magnitude = magnitude
  if (present(unit)) then
    if (allocated(self%unit)) deallocate(self%unit)
    allocate(self%unit, source=unit)
  endif
  if (present(name)) self%name = name
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set

  function stringify(self, format, with_dimensions, with_aliases, with_name, compact_reals) result(raw)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a string representaion of the quantity with unit symbol.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in)           :: self            !< The quantity.
  character(*), intent(in), optional :: format          !< Format to pring magnitude.
  logical,      intent(in), optional :: with_dimensions !< Flag to activate dimensions printing.
  logical,      intent(in), optional :: with_aliases    !< Flag to activate aliases printing.
  logical,      intent(in), optional :: with_name       !< Flag to activate name printing.
  logical,      intent(in), optional :: compact_reals   !< Flag to activate real numbers compacting.
  character(len=:), allocatable      :: raw             !< Raw characters data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  raw = ''
  if (present(with_name)) then
    if (with_name) then
      if (allocated(self%name)) raw = raw//self%name//': '
    endif
  endif
  if (present(format)) then
    raw = raw//trim(str(fm=format, n=self%magnitude))
  else
    raw = raw//trim(str(n=self%magnitude, compact=compact_reals))
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
  real(RKP)                :: factor    !< Whole scale factor.
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
  self%magnitude = 0._RKP
  if (allocated(self%unit)) then
    call self%unit%unset
    deallocate(self%unit)
  endif
  if (allocated(self%name)) deallocate(self%name)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine unset

  ! private methods
  elemental function is_equal(self, other)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if qreal is equal with another one.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: self     !< The unit.
  type(qreal),  intent(in) :: other    !< The other unit.
  logical                  :: is_equal !< Equality check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_equal = (self%magnitude==other%magnitude)
  if (is_equal) then
    if (self%is_unit_defined().and.other%is_unit_defined()) then
      is_equal = (self%unit==other%unit)
    elseif ((self%is_unit_defined().and.(.not.other%is_unit_defined())).or.&
            (.not.self%is_unit_defined().and.(other%is_unit_defined()))) then
      is_equal = .false.
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_equal

  elemental function is_not_equal(self, other)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if qreal is not equal with another one.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: self         !< The unit.
  type(qreal),  intent(in) :: other        !< The other unit.
  logical                  :: is_not_equal !< Disequality check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_not_equal = .not.self%is_equal(other=other)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_not_equal

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
      call lhs%set(magnitude=rhs%magnitude, unit=rhs%unit, name=rhs%name)
    elseif (lhs%unit == rhs%unit) then
      call lhs%set(magnitude=rhs%magnitude, name=rhs%name)
    else
      write(stderr, "(A)") 'error: cannot convert from "'//lhs%stringify(with_dimensions=.true.)//'" to "'//&
                           rhs%stringify(with_dimensions=.true.)//'"'
      stop
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
      write(stderr, "(A)") 'error: cannot convert from "'//lhs%stringify(with_dimensions=.true.)//'" to "'//&
                           rhs%stringify(with_dimensions=.true.)//'"'
      stop
    endif
  else
    ! dimensionless quantities assumed
    opr%magnitude = lhs%magnitude + rhs%magnitude
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction add

  function positive(self) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< ` + qreal` unary operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: self !< The quantity.
  type(qreal)              :: opr  !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr%magnitude = + self%magnitude
  if (self%is_unit_defined()) allocate(opr%unit, source=self%unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction positive

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

  function div_R16P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal / real(R16P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  real(R16P),   intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  opr%magnitude = lhs%magnitude / rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction div_R16P

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

  function mul_R16P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal * real(R16P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  real(R16P),   intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  opr%magnitude = lhs%magnitude * rhs
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction mul_R16P

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

  function R16P_mul(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `real(R16P) * qreal` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  real(R16P),   intent(in) :: lhs !< Left hand side.
  class(qreal), intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = rhs
  opr%magnitude = lhs * rhs%magnitude
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction R16P_mul

  function R8P_mul(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `real(R8P) * qreal` operator.
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
  !< `real(R4P) * qreal` operator.
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
  !< `integer(I8P) * qreal` operator.
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
  !< `integer(I4P) * qreal` operator.
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
  !< `integer(I2P) * qreal` operator.
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
  !< `integer(I1P) * qreal` operator.
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
      write(stderr, "(A)") 'error: cannot convert from "'//lhs%stringify(with_dimensions=.true.)//'" to "'//&
                           rhs%stringify(with_dimensions=.true.)//'"'
      stop
    endif
  else
    ! dimensionless quantities assumed
    opr%magnitude = lhs%magnitude - rhs%magnitude
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction sub

  function negative(self) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< ` - qreal` unary operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: self !< The quantity.
  type(qreal)              :: opr  !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr%magnitude = - self%magnitude
  if (self%is_unit_defined()) allocate(opr%unit, source=self%unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction negative

  function pow_R16P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal ** real(R16P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  real(R16P),   intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr%magnitude = lhs%magnitude ** rhs
  if (lhs%is_unit_defined()) then
    allocate(opr%unit)
    opr%unit = lhs%unit ** rhs
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_R16P

  function pow_R8P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal ** real(R8P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  real(R8P),    intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr%magnitude = lhs%magnitude ** rhs
  if (lhs%is_unit_defined()) then
    allocate(opr%unit)
    opr%unit = lhs%unit ** rhs
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_R8P

  function pow_R4P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `qreal ** real(R4P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(qreal), intent(in) :: lhs !< Left hand side.
  real(R4P),    intent(in) :: rhs !< Right hand side.
  type(qreal)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr%magnitude = lhs%magnitude ** rhs
  if (lhs%is_unit_defined()) then
    allocate(opr%unit)
    opr%unit = lhs%unit ** rhs
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_R4P

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
