!< FURY class definition of unit reference.
module fury_uom_reference
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY class definition of unit reference.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : stderr => error_unit
use fury_uom_converter
use fury_uom_symbol
use penf, RKP => R_P
use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: uom_reference
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: uom_reference
  !< Unit of measure reference class.
  !<
  !< It is the *reference* unit class composed by only 1 symbol, but with many possible aliases that can be used for conversions.
  !<
  !< Provide math operations on symbols necessary to build complex (derived) units.
  !<
  !< The string format definition of a valid FURY unit reference definition is as following:
  !<
  !< `uom_symbolA = uom_symbolB = uom_symbolC = ... [dimensionsA]`
  !<
  !< where
  !<
  !<+ `uom_symbolA` is the main symbol of the unit reference; it is stored as the first reference alias and must respect:
  !<   + `uom_symbolA%offset_ = 0`
  !<   + `uom_symbolA%factor_ = 1`
  !<+ `uom_symbolB, uom_symbolC, ...` are the defined aliases and they are optional; they could be totally general with offset
  !<  and factor used for conversion formulas, until they respect the [[uom_symbol]] syntax;
  !<+ `[dimensions]` is the last optional term that defines the symbol dimensions (if dimensions exponent is passed it must be
  !<  equal to the one of the main symbol.
  !<
  !< For example, valid definition are:
  !<
  !<+ `s-1 = Hz = hertz [time-1]`
  !<+ `kHz< = 1000.0 * Hz = kilohertz> [frequency]`
  !<+ `degC< = celsius = 273.15 + K> [temperature]`
  !<
  !< These terms can be separated by any white spaces number (even zero), but the dimensions must be enclosed into `[]` brackets
  !< at the end of the string.
  type(uom_symbol), allocatable, private :: aliases(:)           !< Uom symbol aliases, e.g. "m = meter = metre" for metres.
  integer(I_P),                  private :: aliases_number=0_I_P !< Number of defined symbol aliases.
  type(uom_symbol),              private :: dimensions           !< Dimensions of the symbol, e.g. "length" for meter.
  contains
    ! public methods
    procedure, pass(self) :: dimensionality             !< Return a string representation of the symbol dimensions.
    ! procedure, pass(self) :: get_aliases                !< Return the aliases list.
    ! procedure, pass(self) :: get_first_compatible_alias !< Get first alias compatible with symbol queried.
    procedure, pass(self) :: get_main_symbol            !< Return the main symbol, i.e. aliases(1).
    procedure, pass(self) :: has_alias                  !< Check if the reference has the queried alias.
    procedure, pass(self) :: has_dimensions             !< Check if the reference dimensions has been defined.
    procedure, pass(self) :: is_defined                 !< Check if the reference is defined.
    procedure, pass(self) :: parse                      !< Parse reference from string.
    procedure, pass(self) :: prefixed                   !< Return a prefixed reference.
    procedure, pass(self) :: set                        !< Set reference.
    procedure, pass(self) :: set_alias_conversion       !< Set alias conversion formula.
    procedure, pass(self) :: stringify                  !< Return a string representation of the reference.
    procedure, pass(self) :: to                         !< Convert magnitude with respect another alias.
    procedure, pass(self) :: unset                      !< Unset reference.
    ! public generic names
    generic :: assignment(=) => assign_uom_reference   !< Overloading `=` assignment.
    generic :: operator(/) => div                      !< Overloading `/` operator.
    generic :: operator(*) => mul                      !< Overloading `*` operator.
    generic :: operator(**) =>                   &
#ifdef r16p
                               pow_R16P,         &
#endif
                               pow_R8P, pow_R4P, &
                               pow_I8P, pow_I4P, &
                               pow_I2P, pow_I1P        !< Overloading `**` operator.
    generic :: operator(==) => is_equal                !< Overloading `==` operator.
    generic :: operator(/=) => is_not_equal            !< Overloading `/=` operator.
    generic :: operator(.compatible.) => is_compatible !< Definition of `.compatible.` operator.
    ! private methods
    procedure, pass(self), private :: is_compatible        !< Check if the reference is compatible with another one.
    procedure, pass(self), private :: is_equal             !< Check if the reference is equal with another one.
    procedure, pass(self), private :: is_not_equal         !< Check if the reference is not equal with another one.
    procedure, pass(lhs),  private :: assign_uom_reference !< `uom_reference = uom_reference` assignment.
    procedure, pass(lhs),  private :: div                  !< `uom_reference / uom_reference` operator.
    procedure, pass(lhs),  private :: mul                  !< `uom_reference * uom_reference` operator.
    procedure, pass(lhs),  private :: pow_R16P             !< `uom_reference ** real(R16P)` operator.
    procedure, pass(lhs),  private :: pow_R8P              !< `uom_reference ** real(R8P)` operator.
    procedure, pass(lhs),  private :: pow_R4P              !< `uom_reference ** real(R4P)` operator.
    procedure, pass(lhs),  private :: pow_I8P              !< `uom_reference ** integer(I8P)` operator.
    procedure, pass(lhs),  private :: pow_I4P              !< `uom_reference ** integer(I4P)` operator.
    procedure, pass(lhs),  private :: pow_I2P              !< `uom_reference ** integer(I2P)` operator.
    procedure, pass(lhs),  private :: pow_I1P              !< `uom_reference ** integer(I1P)` operator.
endtype uom_reference

interface uom_reference
  !< Overloading [[uom_reference]] name with a creator function.
  module procedure creator_from_string
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! private non type bound procedures
  function creator_from_string(source) result(reference)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Create an instance of [[uom_reference]].
  !---------------------------------------------------------------------------------------------------------------------------------
  character(*), intent(in) :: source    !< Source input string definition of the symbol.
  type(uom_reference)      :: reference !< The uom reference.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call reference%parse(source=source)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction creator_from_string

  ! public methods
  pure function dimensionality(self) result(raw)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a string representation of [[uom_reference]] dimensions.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: self !< The uom reference.
  character(len=:), allocatable    :: raw  !< Raw characters data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  raw = ''
  if (self%is_defined().and.self%dimensions%is_defined()) raw = raw//self%dimensions%stringify()
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction dimensionality

  pure function get_aliases(self) result(aliases)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the aliases list.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: self       !< The uom reference.
  type(string), allocatable        :: aliases(:) !< Aliases.
  integer(I_P)                     :: a          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_defined()) then
      allocate(aliases(1:self%aliases_number))
      do a=1, self%aliases_number
        aliases(a) = self%aliases(a)%get_symbol()
      enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction get_aliases

  pure function get_first_compatible_alias(self, other) result(alias)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get first alias-pairs compatible.
  !<
  !< The result is converted to `self%aliases(1)'.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in)  :: self  !< The uom reference.
  type(uom_reference),  intent(in)  :: other !< Alias symbol queried.
  type(uom_symbol)                  :: alias !< First compatible alias.
  integer(I_P)                      :: o     !< Counter.
  integer(I_P)                      :: s     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_defined().and.other%is_defined()) then
    outer_loop: do o=1, other%aliases_number
      do s=1, self%aliases_number
        if (self%aliases(s).compatible.other%aliases(o)) then
          alias = other%aliases(o)
          exit outer_loop
        endif
      enddo
    enddo outer_loop
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction get_first_compatible_alias

  pure function get_main_symbol(self) result(alias)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the main symbol, i.e. aliases(1).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in)  :: self  !< The uom reference.
  type(uom_symbol)                  :: alias !< First compatible alias.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_defined()) alias = self%aliases(1)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction get_main_symbol

  elemental function has_alias(self, alias)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if [[uom_reference]] has the queried alias.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: self      !< The uom reference.
  type(uom_symbol),     intent(in) :: alias     !< Alias queried.
  logical                          :: has_alias !< Check result.
  integer(I_P)                     :: a         !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  has_alias = .false.
  if (self%is_defined()) then
    do a=1, self%aliases_number
      has_alias = (alias==self%aliases(a))
      if (has_alias) exit
    enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction has_alias

  elemental function has_dimensions(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if [[uom_reference]] has defined dimensions.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: self           !< The uom reference.
  logical                          :: has_dimensions !< Check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  has_dimensions = .false.
  if (self%is_defined()) has_dimensions = self%dimensions%is_defined()
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction has_dimensions

  elemental function is_defined(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if [[uom_reference]] is defined.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: self       !< The uom reference.
  logical                          :: is_defined !< Check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_defined = allocated(self%aliases)
  if (is_defined) is_defined = self%aliases(1)%is_defined()
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_defined

  subroutine parse(self, source)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse [[uom_reference]] definition from string.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(inout) :: self      !< The uom reference.
  character(*),         intent(in)    :: source    !< Source input string definition of symbol.
  type(string)                        :: buffer    !< String buffer.
  type(string), allocatable           :: tokens(:) !< String tokens.
  integer(I_P)                        :: a         !< Counter.
  integer(I_P)                        :: d(2)      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%unset
  buffer = trim(adjustl(source))
  d(1) = buffer%index(substring='[')
  d(2) = buffer%index(substring=']')
  if ((d(1)>0).and.(d(2)>0).and.(d(2)>d(1)+1)) then
    call buffer%split(sep='[', tokens=tokens)
    tokens(2) = tokens(2)%slice(1, tokens(2)%index(']')-1)
    call self%dimensions%set(exponent_ = 0_I_P)
    call self%dimensions%parse(source=tokens(2)%chars())
    buffer = tokens(1)
  endif
  self%aliases_number = buffer%count('=') + 1
  call buffer%split(sep='=', tokens=tokens)
  allocate(self%aliases(1:self%aliases_number))
  do a=1, self%aliases_number
    call self%aliases(a)%parse(source=tokens(a)%chars())
  enddo
  if (self%dimensions%get_exponent()/=0.and.self%dimensions%get_exponent()/=self%aliases(1)%get_exponent()) then
    write(stderr, '(A)')'error: parse string definition "'//trim(adjustl(source))//'" failed! '//&
    ' the exponent of the uom and the one of the dimensions (if passed) must be the same!'
    write(stderr, '(A)')'  dimensions: '//self%dimensions%stringify()
    write(stderr, '(A)')'  uom symbol: '//self%aliases(1)%stringify()
    stop
  else
    call self%dimensions%set(exponent_ = self%aliases(1)%get_exponent())
    call self%dimensions%set(factor_ = 1._RKP)
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse

  elemental function prefixed(self, prefixes)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a prefixed reference.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: self       !< The uom reference.
  type(uom_reference),  intent(in) :: prefixes   !< Other reference used for prefixing.
  type(uom_reference)              :: prefixed   !< The prefixed reference.
  type(uom_symbol), allocatable    :: aliases(:) !< Uom symbol aliases, e.g. "m = meter = metre" for metres.
  integer(I_P)                     :: a          !< Counter.
  integer(I_P)                     :: p          !< Counter.
  integer(I_P)                     :: s          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_defined().and.(prefixes%is_defined())) then
    prefixed = self
    allocate(aliases(1:self%aliases_number*prefixes%aliases_number + self%aliases_number))
    a = 0
    do p=1, prefixes%aliases_number
      do s=1, self%aliases_number
        a = a + 1
        aliases(a) = self%aliases(s)%prefixed(prefix=prefixes%aliases(p))
        call aliases(a)%set(factor_=1._RKP)
      enddo
    enddo
    do s=1, self%aliases_number
      a = s + self%aliases_number*prefixes%aliases_number
      aliases(a) = self%aliases(s)
      call aliases(a)%set(factor_=prefixes%aliases(1)%get_factor())
    enddo
    call prefixed%set(aliases=aliases)
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction prefixed

  pure subroutine set(self, aliases, dimensions)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set reference.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(inout)        :: self        !< The uom reference.
  type(uom_symbol),     intent(in), optional :: aliases(1:) !< Reference aliases.
  type(uom_symbol),     intent(in), optional :: dimensions  !< Dimensions of the reference, e.g. "frequency" for Hz.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(aliases)) then
    if (allocated(self%aliases)) deallocate(self%aliases)
    self%aliases_number = size(aliases, dim=1)
    allocate(self%aliases(1:self%aliases_number))
    self%aliases = aliases
  endif
  if (present(dimensions)) self%dimensions = dimensions
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set

  pure subroutine set_alias_conversion(self, alias_index, convert)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set alias conversion formula.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(inout) :: self        !< The uom reference.
  integer(I_P),         intent(in)    :: alias_index !< Index of the alias to which set the conversion formula.
  class(converter),     intent(in)    :: convert     !< Generic conversion alias formula user-supplied.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_defined()) then
    if (alias_index==1) then
      ! TODO implement the error raising for trying to define a main alias with a conversion formula different from identity
    endif
    call self%aliases(alias_index)%set(convert_=convert)
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set_alias_conversion

  pure function stringify(self, with_dimensions, with_aliases, protect_aliases, compact_reals) result(raw)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a string representation of [[uom_reference]].
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in)           :: self            !< The uom reference.
  logical,              intent(in), optional :: with_dimensions !< Flag to activate dimensions printing.
  logical,              intent(in), optional :: with_aliases    !< Flag to activate aliases printing.
  logical,              intent(in), optional :: protect_aliases !< Flag to activate aliases printing in protected mode.
  logical,              intent(in), optional :: compact_reals   !< Flag to activate real numbers compacting.
  character(len=:), allocatable              :: raw             !< Raw characters data.
  integer(I_P)                               :: a               !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  raw = ''
  if (self%is_defined()) then
    raw = raw//self%aliases(1)%stringify(compact_reals=compact_reals)
    if (present(with_aliases)) then
      if (with_aliases.and.self%aliases_number>1) then
        if (present(protect_aliases)) then
          if (protect_aliases) raw = raw//' <'
        endif
        do a=2, self%aliases_number
          raw = raw//' = '//self%aliases(a)%stringify(compact_reals=compact_reals)
        enddo
        if (present(protect_aliases)) then
          if (protect_aliases) raw = raw//' >'
        endif
      endif
    endif
    if (present(with_dimensions)) then
      if (with_dimensions) raw = raw//' ['//self%dimensionality()//']'
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction stringify

  elemental subroutine to(self, other, magnitude, converted, is_found)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Convert magnitude with respect another alias.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in)  :: self      !< The uom reference.
  type(uom_reference),  intent(in)  :: other     !< Other unit reference used for conversion.
  real(RKP),            intent(in)  :: magnitude !< Magnitude to be converted.
  real(RKP),            intent(out) :: converted !< Converted magnitude.
  logical,              intent(out) :: is_found  !< Flag to check if a conversion alias has been found.
  integer(I_P)                      :: a         !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  converted = magnitude
  is_found = .false.
  if (self%is_defined().and.other%is_defined()) then
    direct_conversion: do a=1, self%aliases_number
      if (other%aliases(1).convertible.self%aliases(a)) then
        converted = self%aliases(a)%convert(magnitude=magnitude)
        is_found = .true.
        exit direct_conversion
      endif
    enddo direct_conversion
    if (.not.is_found) then
      inverse_conversion: do a=1, other%aliases_number
        if (other%aliases(a).convertible.self%aliases(1)) then
          converted = other%aliases(a)%convert(magnitude=magnitude, inverse=.true.)
          is_found = .true.
          exit inverse_conversion
        endif
      enddo inverse_conversion
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine to

  elemental subroutine unset(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Unset reference.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(inout) :: self !< The uom reference.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%aliases)) deallocate(self%aliases)
  self%aliases_number = 0_I_P
  call self%dimensions%unset
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine unset

  ! private methods
  elemental function is_compatible(self, other)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if [[uom_reference]] is compatible with another one.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: self          !< The uom reference.
  type(uom_reference),  intent(in) :: other         !< The other reference.
  logical                          :: is_compatible !< Check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_compatible = .false.
  if (self%is_defined().and.other%is_defined()) is_compatible = (self%aliases(1).compatible.other%aliases(1))
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_compatible

  elemental function is_equal(self, other)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if [[uom_reference]] is equal with another one.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: self     !< The uom reference.
  type(uom_reference),  intent(in) :: other    !< The other reference.
  logical                          :: is_equal !< Check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_equal = .false.
  if (self%is_defined().and.other%is_defined()) is_equal = (self%aliases(1)==other%aliases(1))
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_equal

  elemental function is_not_equal(self, other)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if [[uom_reference]] is not equal with another one.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: self         !< The uom reference.
  type(uom_reference),  intent(in) :: other        !< The other reference.
  logical                          :: is_not_equal !< Check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_not_equal = .not.self%is_equal(other=other)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_not_equal

  ! operators
  pure subroutine assign_uom_reference(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference = uom_reference` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(inout) :: lhs !< Left hand side.
  type(uom_reference),  intent(in)    :: rhs !< Right hand side.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (rhs%is_defined())  then
    call lhs%unset
    lhs%aliases_number = size(rhs%aliases, dim=1)
    allocate(lhs%aliases(1:lhs%aliases_number))
    lhs%aliases = rhs%aliases
    lhs%dimensions = rhs%dimensions
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_uom_reference

  pure function div(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference / uom_reference` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: lhs !< Left hand side.
  type(uom_reference),  intent(in) :: rhs !< Right hand side.
  type(uom_reference)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs.compatible.rhs) then
    opr%aliases_number = 1
    allocate(opr%aliases(1))
    opr%aliases(1) = lhs%aliases(1) / rhs%aliases(1)
    opr%dimensions = lhs%dimensions / rhs%dimensions
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction div

  pure function mul(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference * uom_reference` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: lhs !< Left hand side.
  type(uom_reference),  intent(in) :: rhs !< Right hand side.
  type(uom_reference)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs.compatible.rhs) then
    opr%aliases_number = 1
    allocate(opr%aliases(1))
    opr%aliases(1) = lhs%aliases(1) * rhs%aliases(1)
    opr%dimensions = lhs%dimensions * rhs%dimensions
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction mul

  pure function pow_R16P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference ** real(R16P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: lhs !< Left hand side.
  real(R16P),           intent(in) :: rhs !< Right hand side.
  type(uom_reference)              :: opr !< Operator result.
  integer(I_P)                     :: a   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_defined()) then
    opr = lhs
    do a=1, opr%aliases_number
      opr%aliases(a) = opr%aliases(a) ** rhs
    enddo
    opr%dimensions = opr%dimensions ** rhs
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_R16P

  pure function pow_R8P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference ** real(R8P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: lhs !< Left hand side.
  real(R8P),            intent(in) :: rhs !< Right hand side.
  type(uom_reference)              :: opr !< Operator result.
  integer(I_P)                     :: a   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_defined()) then
    opr = lhs
    do a=1, opr%aliases_number
      opr%aliases(a) = opr%aliases(a) ** rhs
    enddo
    opr%dimensions = opr%dimensions ** rhs
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_R8P

  pure function pow_R4P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference ** real(R4P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: lhs !< Left hand side.
  real(R4P),            intent(in) :: rhs !< Right hand side.
  type(uom_reference)              :: opr !< Operator result.
  integer(I_P)                     :: a   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_defined()) then
    opr = lhs
    do a=1, opr%aliases_number
      opr%aliases(a) = opr%aliases(a) ** rhs
    enddo
    opr%dimensions = opr%dimensions ** rhs
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_R4P

  pure function pow_I8P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference ** integer(I8P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: lhs !< Left hand side.
  integer(I8P),         intent(in) :: rhs !< Right hand side.
  type(uom_reference)              :: opr !< Operator result.
  integer(I_P)                     :: a   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_defined()) then
    opr = lhs
    do a=1, opr%aliases_number
      opr%aliases(a) = opr%aliases(a) ** rhs
    enddo
    opr%dimensions = opr%dimensions ** rhs
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I8P

  pure function pow_I4P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference ** integer(I4P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: lhs !< Left hand side.
  integer(I4P),         intent(in) :: rhs !< Right hand side.
  type(uom_reference)              :: opr !< Operator result.
  integer(I_P)                     :: a   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_defined()) then
    opr = lhs
    do a=1, opr%aliases_number
      opr%aliases(a) = opr%aliases(a) ** rhs
    enddo
    opr%dimensions = opr%dimensions ** rhs
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I4P

  pure function pow_I2P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference ** integer(I2P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: lhs !< Left hand side.
  integer(I2P),         intent(in) :: rhs !< Right hand side.
  type(uom_reference)              :: opr !< Operator result.
  integer(I_P)                     :: a   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_defined()) then
    opr = lhs
    do a=1, opr%aliases_number
      opr%aliases(a) = opr%aliases(a) ** rhs
    enddo
    opr%dimensions = opr%dimensions ** rhs
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I2P

  pure function pow_I1P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference ** integer(I1P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: lhs !< Left hand side.
  integer(I1P),         intent(in) :: rhs !< Right hand side.
  type(uom_reference)              :: opr !< Operator result.
  integer(I_P)                     :: a   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_defined()) then
    opr = lhs
    do a=1, opr%aliases_number
      opr%aliases(a) = opr%aliases(a) ** rhs
    enddo
    opr%dimensions = opr%dimensions ** rhs
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I1P
endmodule fury_uom_reference
