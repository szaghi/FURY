!< FURY class definition of unit symbol.
module fury_uom_symbol
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY class definition of unit symbol.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : stderr => error_unit
use fury_uom_converter
use penf, IKP => I_P, RKP => R_P
use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: uom_symbol
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: uom_symbol
  !< Unit of measure (UOM) symbol.
  !<
  !< It is the *base* unit class composed by only 1 symbol.
  !<
  !< Provide math operations on symbols necessary to build complex (derived) units.
  !<
  !< The string format definition of a valid FURY unit symbol definition are as following:
  !<
  !<+ multiplicative-like definition: `real_offset + real_factor * litteral_symbol integer_exponent`
  !<+ generic user-supplied definition: `@user litteral_symbol integer_exponent`
  !<
  !< where
  !<
  !<+`real_offset + ` is an optional real literal constant that, if present, must be followed by a `+`;
  !<+`real_factor + ` is an optional real literal constant that, if present, must be followed by a `*`;
  !<+`litteral_symbol` is the literal symbol;
  !<+`integer_exponent` is an optional integer literal constant that:
  !<   + could be omitted if it is equal to 1;
  !<   + do not have the `+` sign if positive;
  !<   + must have the `-` sign if negative;
  !<+`@user ` is a place order for the parser indicating that the alias-conversion definition is not a simple
  !<+ multiplicative-like conversion, but it is a generic user-supplied one.
  !<
  !< For example, valid definition are:
  !<
  !<+ `s`: a *second* definition; `real_offset` is omitted because equal to 0 and `real_factor` and `integer_exponent` are
  !<  omitted because equal to 1;
  !<+ `1.E6 * m2`: a *square kilometer* definition;
  !<+ `1000 * s-1`: a *kiloherhz* definition;
  !<+ `273.15 + K`: a *Celsius degree* definition;
  !<+ `dBm = @user mW` where the placeholder `@user` will be set (not by the string parser) to `10 * log(mW)`.
  !<
  !< The terms composing a definition can be separated by any white spaces number (even zero).
  !<
  !< In the case the user supply a generic conversion alias formula this one overrides the multiplicative-like optionally parsed.
  integer(IKP),     private              :: exponent_=1_IKP !< Exponent of the symbol, e.g. "-1" for Hertz, namely "s-1".
  real(RKP),        private              :: factor_=1._RKP  !< Symbol multiplicative scale factor (used only for converters).
  real(RKP),        private              :: offset_=0._RKP  !< Symbol additive offset (used only for converters).
  type(string),     private              :: symbol_         !< literal symbol, e.g. "m" for metres.
  class(converter), private, allocatable :: convert_        !< Generic conversion alias formula user-supplied.
  contains
    ! public methods
    procedure, pass(self) :: convert      !< Convert a magnitude with respect symbol definition.
    procedure, pass(self) :: get_exponent !< Return the symbol exponent.
    procedure, pass(self) :: get_factor   !< Return the symbol factor.
    procedure, pass(self) :: get_offset   !< Return the symbol offset.
    procedure, pass(self) :: get_symbol   !< Return the literal symbol.
    procedure, pass(self) :: is_defined   !< Check if the symbol is defined.
    procedure, pass(self) :: parse        !< Parse symbol from string.
    procedure, pass(self) :: prefixed     !< Return a prefixed symbol.
    procedure, pass(self) :: set          !< Set symbol.
    procedure, pass(self) :: stringify    !< Return a string representaion of the symbol.
    ! procedure, pass(self) :: to           !< TO BE ELIMINATED!
    procedure, pass(self) :: unset        !< Unset symbol.
    ! public generic names
    generic :: assignment(=) => assign_uom_symbol        !< Overloading `=` assignment.
    generic :: operator(/) => div                        !< Overloading `/` operator.
    generic :: operator(*) => mul                        !< Overloading `*` operator.
    generic :: operator(**) =>                   &
#ifdef r16p
                               pow_R16P,         &
#endif
                               pow_R8P, pow_R4P, &
                               pow_I8P, pow_I4P, &
                               pow_I2P, pow_I1P          !< Overloading `**` operator.
    generic :: operator(==) => is_equal                  !< Overloading `==` operator.
    generic :: operator(/=) => is_not_equal              !< Overloading `/=` operator.
    generic :: operator(.compatible.) => is_compatible   !< Definition of `.compatible.` operator.
    generic :: operator(.convertible.) => is_convertible !< Definition of `.convertible.` operator.
    ! private methods
    procedure, pass(self), private :: is_compatible     !< Check if the symbol is compatible with another one.
    procedure, pass(self), private :: is_convertible    !< Check if the symbol is convertible with another one.
    procedure, pass(self), private :: is_equal          !< Check if the symbol is equal with another one.
    procedure, pass(self), private :: is_not_equal      !< Check if the symbol is not equal with another one.
    procedure, pass(lhs),  private :: assign_uom_symbol !< `uom_symbol = uom_symbol` assignment.
    procedure, pass(lhs),  private :: div               !< `uom_symbol / uom_symbol` operator.
    procedure, pass(lhs),  private :: mul               !< `uom_symbol * uom_symbol` operator.
    procedure, pass(lhs),  private :: pow_R16P          !< `uom_symbol ** real(R16P)` operator.
    procedure, pass(lhs),  private :: pow_R8P           !< `uom_symbol ** real(R8P)` operator.
    procedure, pass(lhs),  private :: pow_R4P           !< `uom_symbol ** real(R4P)` operator.
    procedure, pass(lhs),  private :: pow_I8P           !< `uom_symbol ** integer(I8P)` operator.
    procedure, pass(lhs),  private :: pow_I4P           !< `uom_symbol ** integer(I4P)` operator.
    procedure, pass(lhs),  private :: pow_I2P           !< `uom_symbol ** integer(I2P)` operator.
    procedure, pass(lhs),  private :: pow_I1P           !< `uom_symbol ** integer(I1P)` operator.
endtype uom_symbol

interface uom_symbol
  !< Overloading [[uom_symbol]] name with a creator function.
  module procedure creator_from_string
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! private non type bound procedures
  function creator_from_string(source) result(symbol)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Create an instance of [[uom_symbol]].
  !---------------------------------------------------------------------------------------------------------------------------------
  character(*), intent(in) :: source !< Source input string definition of the symbol.
  type(uom_symbol)         :: symbol !< The uom symbol.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call symbol%parse(source=source)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction creator_from_string

  ! public methods
  elemental function convert(self, magnitude, inverse) result(converted)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Convert a magnitude with respect symbol definition.
  !<
  !< Two conversions are possible: direct and inverse, namely
  !<
  !<+ direct conversion:  `converted = offset+ factor * magnitude`
  !<+ inverse conversion: `converted = (magnitude - offset)/factor`
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(in)           :: self       !< The uom symbol.
  real(RKP),         intent(in)           :: magnitude  !< Magnitude to be converted.
  logical,           intent(in), optional :: inverse    !< Activate inverse conversion.
  real(RKP)                               :: converted  !< Converted magnitude.
  logical                                 :: inverse_   !< Activate inverse conversion, local variable.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  converted = magnitude
  if (self%is_defined()) then
    if (allocated(self%convert_)) then
      converted = self%convert_%convert(magnitude=magnitude, inverse=inverse)
    else
      inverse_ = .false. ; if (present(inverse)) inverse_ = inverse
      if (inverse_) then
        converted = (magnitude - self%offset_) / self%factor_
      else
        converted = self%offset_ + self%factor_ * magnitude
      endif
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction convert

  elemental function get_exponent(self) result(exponent_)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the symbol exponent.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(in) :: self      !< The uom symbol.
  integer(IKP)                  :: exponent_ !< The symbol exponent.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_defined()) exponent_ = self%exponent_
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction get_exponent

  elemental function get_factor(self) result(factor_)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the symbol factor.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(in) :: self    !< The uom symbol.
  real(RKP)                     :: factor_ !< The symbol factor.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_defined()) factor_ = self%factor_
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction get_factor

  elemental function get_offset(self) result(offset_)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the symbol offset.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(in) :: self    !< The uom symbol.
  real(RKP)                     :: offset_ !< The symbol offset.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_defined()) offset_ = self%offset_
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction get_offset

  elemental function get_symbol(self) result(symbol_)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the literal symbol.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(in) :: self !< The uom symbol.
  type(string)                  :: symbol_ !< The literal symbol.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_defined()) symbol_ = self%symbol_
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction get_symbol

  elemental function is_defined(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if [[uom_symbol]] is defined.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(in) :: self       !< The uom symbol.
  logical                       :: is_defined !< Check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_defined = self%symbol_%is_allocated()
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_defined

  subroutine parse(self, source)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse symbol definition from string.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(inout) :: self      !< The uom symbol.
  character(*),      intent(in)    :: source    !< Source input string definition of symbol.
  type(string)                     :: buffer    !< String buffer.
  type(string), allocatable        :: tokens(:) !< String tokens.
  integer(IKP)                     :: e         !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  buffer = trim(adjustl(source))
  if (buffer%count('@user') > 0) then
    buffer = buffer%replace(old='@user', new='')
  else
    if (buffer%count('+') > 0) then
      call buffer%split(sep='+', tokens=tokens)
      self%offset_ = cton(str=tokens(1)%chars(), knd=1._RKP)
      buffer = tokens(2)
    endif
    if (buffer%count('*') > 0) then
      call buffer%split(sep='*', tokens=tokens)
      self%factor_ = cton(str=tokens(1)%chars(), knd=1._RKP)
      if (self%factor_==0._RKP) then
        write(stderr, "(A)") 'error: symbol cannot have "'//trim(str(n=self%factor_, compact=.true.))//'" multiplicative factor'
        stop
      endif
      buffer = tokens(2)
    endif
  endif
  e = buffer%scan(set='-0123456789')
  if (e>0) then
    self%exponent_ = cton(str=buffer%slice(e, buffer%len()), knd=1_IKP)
    buffer = buffer%slice(1, e-1)
  endif
  self%symbol_ = trim(adjustl(buffer%chars()))
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse

  elemental function prefixed(self, prefix)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a prefixed symbol.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(in) :: self     !< The uom symbol.
  type(uom_symbol),  intent(in) :: prefix   !< Other symbol used for prefixing.
  type(uom_symbol)              :: prefixed !< The prefixed symbol.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_defined().and.(prefix%is_defined())) then
    prefixed%symbol_ = prefix%symbol_//self%symbol_
    prefixed%exponent_ = self%exponent_
    prefixed%factor_ = self%factor_ * prefix%factor_
    prefixed%offset_ = self%offset_
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction prefixed

  elemental subroutine set(self, symbol_, exponent_, factor_, offset_, convert_)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set symbol.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(inout)        :: self       !< The uom symbol.
  character(*),      intent(in), optional :: symbol_    !< literal symbol of the unit, e.g. "m" for metres.
  integer(IKP),      intent(in), optional :: exponent_  !< Exponent of the symbol, e.g. "-1" for Hertz, namely "s-1".
  real(RKP),         intent(in), optional :: factor_    !< Symbol multiplicative scale factor (used only for converters).
  real(RKP),         intent(in), optional :: offset_    !< Symbol additive offset (used only for converters).
  class(converter),  intent(in), optional :: convert_   !< Generic conversion alias formula user-supplied.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(symbol_)) self%symbol_ = symbol_
  if (present(exponent_)) self%exponent_ = exponent_
  if (present(factor_)) self%factor_ = factor_
  if (present(offset_)) self%offset_ = offset_
  if (present(convert_)) then
    if (allocated(self%convert_)) deallocate(self%convert_)
    allocate(self%convert_, source=convert_)
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set

  pure function stringify(self, compact_reals) result(raw)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a string representation of [[uom_symbol]].
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(in)           :: self          !< The uom symbol.
  logical,           intent(in), optional :: compact_reals !< Flag to activate real numbers compacting.
  character(len=:), allocatable           :: raw           !< Raw characters data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  raw = ''
  if (self%is_defined()) then
    if (self%offset_/=0._RKP) then
      raw = raw//trim(str(n=self%offset_, compact=compact_reals))//' + '
    endif
    if (self%factor_/=1._RKP) then
      raw = raw//trim(str(n=self%factor_, compact=compact_reals))//' * '
    endif
    raw = raw//self%symbol_
    if (self%exponent_<0) then
      raw = raw//trim(str(n=self%exponent_))
    elseif (self%exponent_/=1_IKP) then
      raw = raw//trim(str(n=self%exponent_, no_sign=.true.))
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction stringify

  pure function to(self, other)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< TO BE ELIMINATED!
  !< Convert symbol to another.
  !<
  !< It is assumed a multiplicative-like conversion from the equivalence:
  !<
  !< `self%offset_ + self%factor_ * self%symbol_**self%exponent_ = other%offset_ + other%factor_ * other%symbol_**other%exponent_=>`
  !< `=> to%symbol_ = other%symbol_`
  !< `=> to%exponent_ = other%exponent_`
  !< `=> to%factor_ = self%factor_ / other%factor_`
  !< `=> to%offset_ = (self%factor_ - other%offset_) / other%factor_`
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(in) :: self  !< The uom symbol.
  type(uom_symbol),  intent(in) :: other !< Other symbol used for conversion.
  type(uom_symbol)              :: to    !< The converted symbol.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  to%symbol_ = other%symbol_
  to%exponent_ = other%exponent_
  to%factor_ = self%factor_ / other%factor_
  to%offset_ = (self%factor_ - other%offset_) / other%factor_
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction to

  elemental subroutine unset(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Unset symbol.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(inout) :: self !< The uom symbol.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%symbol_%free
  self%exponent_ = 1_IKP
  self%factor_ = 1._RKP
  self%offset_ = 0._RKP
  if (allocated(self%convert_)) deallocate(self%convert_)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine unset

  ! private methods
  elemental function is_compatible(self, other)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if [[uom_symbol]] is compatible with another one.
  !<
  !< Two symbols are defined *compatible* if they have the same literal symbol, unconcerned other attributes.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(in) :: self          !< The uom symbol.
  type(uom_symbol),  intent(in) :: other         !< The other symbol.
  logical                       :: is_compatible !< Check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_compatible = .false.
  if (self%is_defined().and.other%is_defined()) &
    is_compatible = (self%factor_==other%factor_.and.self%offset_==other%offset_.and.self%symbol_==other%symbol_)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_compatible

  elemental function is_convertible(self, other)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if [[uom_symbol]] is convertible with another one.
  !<
  !< Two symbols are defined *convertible* if they have the same literal symbol, unconcerned other attributes.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(in) :: self          !< The uom symbol.
  type(uom_symbol),  intent(in) :: other         !< The other symbol.
  logical                       :: is_convertible !< Check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_convertible = .false.
  if (self%is_defined().and.other%is_defined()) &
    is_convertible = (self%exponent_==other%exponent_.and.self%symbol_==other%symbol_)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_convertible

  elemental function is_equal(self, other)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if [[uom_symbol]] is equal with another one.
  !<
  !< Two symbols are defined *equal* if all their attributes are identical.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(in) :: self     !< The uom symbol.
  type(uom_symbol),  intent(in) :: other    !< The other symbol.
  logical                       :: is_equal !< Check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_equal = .false.
  if (self%is_defined().and.other%is_defined())                                      &
    is_equal = (self%exponent_==other%exponent_.and.self%factor_==other%factor_.and. &
                self%offset_==other%offset_.and.self%symbol_==other%symbol_)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_equal

  elemental function is_not_equal(self, other)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if [[uom_symbol]] is not equal with another one.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(in) :: self         !< The uom symbol.
  type(uom_symbol),  intent(in) :: other        !< The other symbol.
  logical                       :: is_not_equal !< Check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_not_equal = .not.self%is_equal(other=other)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_not_equal

  ! operators
  pure subroutine assign_uom_symbol(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_symbol = uom_symbol` assignment.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(inout) :: lhs !< Left hand side.
  type(uom_symbol),  intent(in)    :: rhs !< Right hand side.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (rhs%is_defined())  then
    call lhs%unset
    lhs%exponent_ = rhs%exponent_
    lhs%factor_ = rhs%factor_
    lhs%offset_ = rhs%offset_
    lhs%symbol_ = rhs%symbol_
    if (allocated(rhs%convert_)) allocate(lhs%convert_, source=rhs%convert_)
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_uom_symbol

  pure function div(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_symbol / uom_symbol` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(in) :: lhs !< Left hand side.
  type(uom_symbol),  intent(in) :: rhs !< Right hand side.
  type(uom_symbol)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs.compatible.rhs) then
    opr%exponent_ = lhs%exponent_ - rhs%exponent_
    opr%factor_ = lhs%factor_
    opr%offset_ = lhs%offset_
    opr%symbol_ = lhs%symbol_
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction div

  pure function mul(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_symbol * uom_symbol` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(in) :: lhs !< Left hand side.
  type(uom_symbol),  intent(in) :: rhs !< Right hand side.
  type(uom_symbol)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs.compatible.rhs) then
    opr%exponent_ = lhs%exponent_ + rhs%exponent_
    opr%factor_ = lhs%factor_
    opr%offset_ = lhs%offset_
    opr%symbol_ = lhs%symbol_
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction mul

  pure function pow_R16P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_symbol ** real(R16P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(in) :: lhs !< Left hand side.
  real(R16P),        intent(in) :: rhs !< Right hand side.
  type(uom_symbol)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_defined()) then
    opr%exponent_ = lhs%exponent_ * rhs
    opr%factor_ = lhs%factor_ ** rhs
    if (lhs%offset_/=0._RKP) opr%offset_ = lhs%offset_ ** rhs
    opr%symbol_ = lhs%symbol_
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_R16P

  pure function pow_R8P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_symbol ** real(R8P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(in) :: lhs !< Left hand side.
  real(R8P),         intent(in) :: rhs !< Right hand side.
  type(uom_symbol)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_defined()) then
    opr%exponent_ = lhs%exponent_ * rhs
    opr%factor_ = lhs%factor_ ** rhs
    if (lhs%offset_/=0._RKP) opr%offset_ = lhs%offset_ ** rhs
    opr%symbol_ = lhs%symbol_
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_R8P

  pure function pow_R4P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_symbol ** real(R4P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(in) :: lhs !< Left hand side.
  real(R4P),         intent(in) :: rhs !< Right hand side.
  type(uom_symbol)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_defined()) then
    opr%exponent_ = lhs%exponent_ * rhs
    opr%factor_ = lhs%factor_ ** rhs
    if (lhs%offset_/=0._RKP) opr%offset_ = lhs%offset_ ** rhs
    opr%symbol_ = lhs%symbol_
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_R4P

  pure function pow_I8P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_symbol ** integer(I8P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(in) :: lhs !< Left hand side.
  integer(I8P),      intent(in) :: rhs !< Right hand side.
  type(uom_symbol)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_defined()) then
    opr%exponent_ = lhs%exponent_ * rhs
    opr%factor_ = lhs%factor_ ** rhs
    if (lhs%offset_/=0._RKP) opr%offset_ = lhs%offset_ ** rhs
    opr%symbol_ = lhs%symbol_
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I8P

  pure function pow_I4P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_symbol ** integer(I4P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(in) :: lhs !< Left hand side.
  integer(I4P),      intent(in) :: rhs !< Right hand side.
  type(uom_symbol)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_defined()) then
    opr%exponent_ = lhs%exponent_ * rhs
    opr%factor_ = lhs%factor_ ** rhs
    if (lhs%offset_/=0._RKP) opr%offset_ = lhs%offset_ ** rhs
    opr%symbol_ = lhs%symbol_
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I4P

  pure function pow_I2P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_symbol ** integer(I2P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(in) :: lhs !< Left hand side.
  integer(I2P),      intent(in) :: rhs !< Right hand side.
  type(uom_symbol)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_defined()) then
    opr%exponent_ = lhs%exponent_ * rhs
    opr%factor_ = lhs%factor_ ** rhs
    if (lhs%offset_/=0._RKP) opr%offset_ = lhs%offset_ ** rhs
    opr%symbol_ = lhs%symbol_
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I2P

  pure function pow_I1P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_symbol ** integer(I1P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_symbol), intent(in) :: lhs !< Left hand side.
  integer(I1P),      intent(in) :: rhs !< Right hand side.
  type(uom_symbol)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%is_defined()) then
    opr%exponent_ = lhs%exponent_ * rhs
    opr%factor_ = lhs%factor_ ** rhs
    if (lhs%offset_/=0._RKP) opr%offset_ = lhs%offset_ ** rhs
    opr%symbol_ = lhs%symbol_
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I1P
endmodule fury_uom_symbol
