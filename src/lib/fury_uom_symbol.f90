!< FURY class definition of unit reference (composed by only 1 symbol).
module fury_uom_reference
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY class definition of unit reference (composed by only 1 symbol).
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : stderr => error_unit
use penf
use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: parse_uom_references
public :: uom_reference
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: uom_reference
  !< Unit reference class.
  !<
  !< It is the *base* unit class composed by only 1 symbol.
  !<
  !< Provide math operations on symbols necessary to build complex (derived) units.
  !<
  !< The calss is constituted by 2 required and 1 optional members, namely the *litteral* symbol and its exponent and the
  !< (optional) dimensions tag (that obviuosly has the same exponent of the symbol). These 3 members allow the definition of
  !< 3 different kind of comparisons:
  !<
  !<+ *compatibility*: 2 symbols are defined *compatible* if they have the same litteral symbol (and dimensions if defined)
  !<                   unregarded their exponent values; this allows for the *compatible* math operation, e.g. `m2 => m3 / m`
  !<                   where `m3` and `m` are *compatible* in the sense that they can be
  !<                   divided/multiplied, whereas `m / s` is not a compatible operation rather a mixing rule that is handled
  !<                   by the [[unit]] class;
  !<+ *equality*: 2 symbols are defined *equals* if they have the same litteral symbol (and dimensions if defined) and also the
  !<              same exponent value; this allows addition/subtraction operations handled by the [[unit]] class.
  !<+ *dimensionality*: 2 symbols are defined *equals in dimensions* if they have dimensions and also the same exponent value,
  !<                    unregarded their litteral symbols; this allows for units conversions;
  !<
  !< The string format definition of a valid FURY unit reference definition is as following:
  !<
  !< `s-1 = Hz = hertz [time-1]`
  !< `kHz = kilohertz < 1000 * Hz = 1000 * s-1 > [time-1]`
  !<
  !< where
  !<
  !<+ `s-1` is the first mandatory term that defines the litteral symbol with its exponent (if 1 can be omitted);
  !<+ all subsequent ` = Hz = ...` are optional aliases (with their own exponent) of the main litteral symbol;
  !<+ `< 1000 * Hz ...> terms are (optional) *multiplicative-conversion* formulas to convert from on symbol to another that have the same
  !<  dimensions; these formulas must be enclosed into `<>` pairs;
  !<+ `[time-1]` is the last optional term that defines the symbol dimensions (if dimensions exponent is passed it must be equal
  !<  to the one of the main litteral symbol.
  !<
  !< These terms can be separated by any white spaces number (even zero), but the dimensions must be enclosed into `[]` brackets
  !< at the end of the string.
  character(len=:), allocatable, private :: symbol                !< Litteral symbol, e.g. "m" for metres.
  integer(I_P),                  private :: symbol_exponent=1_I_P !< Exponent of the symbol, e.g. "1" for metres, namely "m1".
  real(R_P),                     private :: symbol_scale=1._R_P   !< Symbol multiplicative scale factor (used only for converters).
  character(len=:), allocatable, private :: dimensions            !< Dimensions of the symbol, e.g. "length" for metres.
  type(uom_reference), pointer,  private :: aliases(:)=>null()    !< Litteral symbol, e.g. "meter, meters..." for metres.
  integer(I_P),                  private :: aliases_number=0_I_P  !< Number of defined symbol aliases.
  contains
    ! public methods
    procedure, pass(self) :: dimensionality !< Return a string representation of the symbol dimensionality.
    procedure, pass(self) :: is_compatible  !< Check if the symbol is compatible with another one.
    procedure, pass(self) :: parse          !< Parse symbol from string.
    procedure, pass(self) :: stringify      !< Return a string representaion of the symbol.
    ! public generic names
    generic :: assignment(=) => assign_string, &
                                assign_uom_reference !< Overloading `=` assignament.
    generic :: operator(/) => div                    !< Overloading `/` operator.
    generic :: operator(*) => mul                    !< Overloading `*` operator.
    generic :: operator(**) => pow_I8P, pow_I4P, &
                               pow_I2P, pow_I1P      !< Overloading `**` operator.
    generic :: operator(==) => is_equal              !< Overloading `==` operator.
    generic :: operator(/=) => is_not_equal          !< Overloading `/=` operator.
    ! private methods
    procedure, pass(self), private :: get_alias_scale         !< Get the scale factor from an alias.
    procedure, pass(self), private :: has_aliases             !< Check if the symbol has defined aliase.
    procedure, pass(self), private :: has_dimensions          !< Check if the symbol dimensions has been defined.
    procedure, pass(self), private :: has_symbol              !< Check if the symbol has been defined.
    procedure, pass(self), private :: is_equal                !< Check if the symbol is equal with another one.
    procedure, pass(self), private :: is_not_equal            !< Check if the symbol is not equal with another one.
    procedure, pass(self), private :: parse_symbol            !< Parse symbol definition from string.
    procedure, pass(self), private :: parse_symbol_dimensions !< Parse symbol dimensions definition from string.
    procedure, pass(self), private :: set                     !< Set symbol.
    procedure, pass(self), private :: unset                   !< Unset symbol.
    procedure, pass(lhs),  private :: assign_string           !< `uom_reference = string` assignament.
    procedure, pass(lhs),  private :: assign_uom_reference    !< `uom_reference = uom_reference` assignament.
    procedure, pass(lhs),  private :: div                     !< `uom_reference / uom_reference` operator.
    procedure, pass(lhs),  private :: mul                     !< `uom_reference * uom_reference` operator.
    procedure, pass(lhs),  private :: pow_I8P                 !< `uom_reference ** integer(I8P)` operator.
    procedure, pass(lhs),  private :: pow_I4P                 !< `uom_reference ** integer(I4P)` operator.
    procedure, pass(lhs),  private :: pow_I2P                 !< `uom_reference ** integer(I2P)` operator.
    procedure, pass(lhs),  private :: pow_I1P                 !< `uom_reference ** integer(I1P)` operator.
endtype uom_reference

interface uom_reference
  !< Ovearloading [[uom_reference]] name with a creator function.
  module procedure creator_from_string
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! public non type bound procedures
  function parse_uom_references(source) result(references)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse string returning an array of [[uom_reference]].
  !---------------------------------------------------------------------------------------------------------------------------------
  character(*), intent(in)         :: source        !< Reference litteral symbol(s) of the unit, e.g. "m.s-1" for metres/second.
  type(uom_reference), allocatable :: references(:) !< Reference litteral symbol(s), array var.
  type(string)                     :: buffer        !< String buffer.
  type(string), allocatable        :: tokens(:)     !< String tokens.
  integer(I_P)                     :: tokens_number !< Tokens number.
  integer(I_P)                     :: t             !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  buffer = trim(adjustl(source))
  call buffer%split(tokens=tokens, sep='.')
  tokens_number = size(tokens, dim=1)
  allocate(references(1:tokens_number))
  do t=1, tokens_number
    call references(t)%parse(source=tokens(t)%chars())
  enddo
  ! reduce to unique symbols list
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction parse_uom_references

  ! private non type bound procedures
  function creator_from_string(source) result(symbol)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Create an instance of [[uom_reference]].
  !---------------------------------------------------------------------------------------------------------------------------------
  character(*), intent(in) :: source !< Source input string definition of the symbol.
  type(uom_reference)      :: symbol !< The uom reference.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call symbol%parse(source=source)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction creator_from_string

  ! public methods
  pure function dimensionality(self) result(raw)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a string representation of [[uom_reference]] dimensionality.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: self !< The uom reference.
  character(len=:), allocatable    :: raw  !< Raw characters data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%has_dimensions()) then
    raw = self%dimensions
    if (self%symbol_exponent<0) then
      raw = raw//trim(str(n=self%symbol_exponent))
    elseif (self%symbol_exponent/=1_I_P) then
      raw = raw//trim(str(n=self%symbol_exponent, no_sign=.true.))
    endif
  else
    raw = ''
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction dimensionality

  elemental function is_compatible(self, other) result(compatible)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if [[uom_reference]] is compatible with another one.
  !<
  !< Two symbols are defined *compatible* if they have the same litteral symbol, unregarded their exponent values.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: self       !< The uom reference.
  type(uom_reference),  intent(in) :: other      !< The other symbol.
  logical                          :: compatible !< Compatibility check result.
  integer(I_P)                     :: o          !< Counter.
  integer(I_P)                     :: s          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  compatible = .false.
  if (self%has_symbol().and.other%has_symbol()) then
    compatible = (self%symbol==other%symbol)
    if (.not.compatible.and.other%has_aliases()) then
      do o=1, other%aliases_number
        if (other%aliases(o)%has_symbol()) compatible = (self%symbol==other%aliases(o)%symbol)
        if (compatible) exit
      enddo
    endif
    if (.not.compatible.and.self%has_aliases()) then
      do s=1, self%aliases_number
        if (self%aliases(s)%has_symbol()) compatible = (other%symbol==self%aliases(s)%symbol)
        if (compatible) exit
      enddo
    endif
    if (.not.compatible.and.self%has_aliases()) then
      outer_loop: do o=1, other%aliases_number
        do s=1, self%aliases_number
          compatible = (self%aliases(s)%symbol==other%aliases(o)%symbol)
          if (compatible) exit outer_loop
        enddo
      enddo outer_loop
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_compatible

  subroutine parse(self, source)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse symbol definition from string.
  !<
  !< The string format definition of a valid FURY unit reference definition is as following:
  !<
  !< `s-1 = Hz = hertz [time-1]`
  !< `kHz = kilohertz < 1000 * Hz = 1000 * s-1 > [time-1]`
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(inout) :: self                  !< The uom reference.
  character(*),         intent(in)    :: source                !< Source input string definition of symbol.
  type(string)                        :: buffer                !< String buffer.
  type(string), allocatable           :: tokens(:)             !< String tokens.
  type(uom_reference)                 :: dimensions_symbolized !< Storing dimensions data as a symbol.
  integer(I_P)                        :: a                     !< Counter.
  integer(I_P)                        :: d(2)                  !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ! parse dimensions (with its exponent)
  buffer = trim(adjustl(source))
  call dimensions_symbolized%parse_symbol_dimensions(source=buffer)
  ! allocate aliases of litteral symbol
  self%aliases_number = buffer%count('=')
  if (associated(self%aliases)) deallocate(self%aliases)
  allocate(self%aliases(1:self%aliases_number))
  ! parse main symbol
  call buffer%split(sep='=', tokens=tokens)
  call self%parse_symbol(source=tokens(1))
  if (dimensions_symbolized%symbol_exponent/=0.and.dimensions_symbolized%symbol_exponent/=self%symbol_exponent) then
    write(stderr, '(A)')'error: parse string definition "'//trim(adjustl(source))//'" failed! '//&
    ' the exponent of the symbol and the one of the dimensions (if passed) must be the same!'
    stop
  else
    self%dimensions = dimensions_symbolized%symbol
  endif
  ! parse other aliases of litteral symbol if any
  if (self%aliases_number>0) then
    do a=1, self%aliases_number
      call self%aliases(a)%parse_symbol(source=tokens(a+1))
    enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse

  recursive function stringify(self, with_dimensions, with_aliases, with_conversions, compact_reals) result(raw)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a string representation of [[uom_reference]].
  !<
  !< @todo Add conversion formulas printing.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in)           :: self             !< The uom reference.
  logical,              intent(in), optional :: with_dimensions  !< Flag to activate dimensions printing.
  logical,              intent(in), optional :: with_aliases     !< Flag to activate aliases printing.
  logical,              intent(in), optional :: with_conversions !< Flag to activate conversion formulas printing.
  logical,              intent(in), optional :: compact_reals    !< Flag to activate real numbers compacting.
  character(len=:), allocatable              :: raw              !< Raw characters data.
  integer(I_P)                               :: a                !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  raw = ''
  if (self%has_symbol()) then
    if (self%symbol_scale/=1._R_P) then
      raw = raw//trim(str(n=self%symbol_scale, compact=compact_reals))//' * '
    endif
    raw = raw//self%symbol
    if (self%symbol_exponent<0) then
      raw = raw//trim(str(n=self%symbol_exponent))
    elseif (self%symbol_exponent/=1_I_P) then
      raw = raw//trim(str(n=self%symbol_exponent, no_sign=.true.))
    endif
    if (present(with_dimensions)) then
      if (with_dimensions) raw = raw//' ['//self%dimensionality()//']'
    endif
    if (present(with_aliases)) then
      if (with_aliases.and.self%has_aliases()) then
        do a=1, self%aliases_number
          raw = raw//' = '//self%aliases(a)%stringify(compact_reals=compact_reals)
        enddo
      endif
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction stringify

  recursive subroutine unset(self, only_aliases)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Unset symbol.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(inout)        :: self         !< The uom reference.
  logical,              intent(in), optional :: only_aliases !< Unset only aliases if .true..
  integer(I_P)                               :: a            !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (associated(self%aliases)) then
    do a=1, self%aliases_number
      call self%aliases(a)%unset
    enddo
    deallocate(self%aliases)
    self%aliases => null()
    self%aliases_number = 0_I_P
  endif
  if (present(only_aliases)) then
    if (only_aliases) return
  endif
  if (allocated(self%symbol)) deallocate(self%symbol)
  self%symbol_exponent = 1_I_P
  self%symbol_scale = 1._R_P
  if (allocated(self%dimensions)) deallocate(self%dimensions)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine unset

  ! private methods
  elemental function get_alias_scale(self, alias_symbol) result(scale_factor)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get the scale factor from an alias.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: self         !< The uom reference.
  character(*),         intent(in) :: alias_symbol !< Alias symbol queried.
  real(R_P)                        :: scale_factor !< Symbol scale factor.
  integer(I_P)                     :: a            !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  scale_factor = 1._R_P
  if (self%has_aliases()) then
    do a=1, self%aliases_number
      if (alias_symbol==self%aliases(a)%symbol) then
        scale_factor = self%aliases(a)%symbol_scale
        exit
      endif
    enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction get_alias_scale

  elemental function has_aliases(self) result(is_defined)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if [[uom_reference]] has defined aliases.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: self       !< The uom reference.
  logical                          :: is_defined !< Symbol aliases definition status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_defined = associated(self%aliases)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction has_aliases

  elemental function has_dimensions(self) result(is_defined)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if [[uom_reference]] dimensions has been defined.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: self       !< The uom reference.
  logical                          :: is_defined !< Symbol dimensions definition status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_defined = allocated(self%dimensions)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction has_dimensions

  elemental function has_symbol(self) result(is_defined)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if [[uom_reference]] has been defined.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: self       !< The uom reference.
  logical                          :: is_defined !< Symbol definition status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_defined = allocated(self%symbol)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction has_symbol

  elemental function is_equal(self, other) result(equal)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if [[uom_reference]] is equal with another one.
  !<
  !< Two symbols are defined *equal* if they have the same litteral symbol and the same exponent value.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: self  !< The uom reference.
  type(uom_reference),  intent(in) :: other !< The other symbol.
  logical                          :: equal !< Equality check result.
  integer(I_P)                     :: o     !< Counter.
  integer(I_P)                     :: s     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  equal = .false.
  if (self%has_symbol().and.other%has_symbol()) then
    equal = ((self%symbol==other%symbol).and.&
             (self%symbol_exponent==other%symbol_exponent).and.&
             (self%symbol_scale==other%symbol_scale))
    if (.not.equal.and.other%has_aliases()) then
      do o=1, other%aliases_number
        if (other%aliases(o)%has_symbol()) &
          equal = ((self%symbol==other%aliases(o)%symbol).and.&
                   (self%symbol_exponent==other%aliases(o)%symbol_exponent).and.&
                   (self%symbol_scale==other%aliases(o)%symbol_scale))
        if (equal) exit
      enddo
    endif
    if (.not.equal.and.self%has_aliases()) then
      do s=1, self%aliases_number
        if (self%aliases(s)%has_symbol()) &
          equal = ((other%symbol==self%aliases(s)%symbol).and.&
                   (other%symbol_exponent==self%aliases(s)%symbol_exponent).and.&
                   (other%symbol_scale==self%aliases(s)%symbol_scale))
        if (equal) exit
      enddo
    endif
    if (.not.equal.and.self%has_aliases()) then
      outer_loop: do o=1, other%aliases_number
        do s=1, self%aliases_number
          equal = ((self%aliases(s)%symbol==other%aliases(o)%symbol).and.&
                   (self%aliases(s)%symbol_exponent==other%aliases(o)%symbol_exponent).and.&
                   (self%aliases(s)%symbol_scale==other%aliases(o)%symbol_scale))
          if (equal) exit outer_loop
        enddo
      enddo outer_loop
    endif
    if (equal.and.self%has_dimensions().and.other%has_dimensions()) equal = (self%dimensions==other%dimensions)
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_equal

  elemental function is_not_equal(self, other) result(not_equal)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if [[uom_reference]] is not equal with another one.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: self      !< The uom reference.
  type(uom_reference),  intent(in) :: other     !< The other symbol.
  logical                          :: not_equal !< Disequality check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  not_equal = .not.self%is_equal(other=other)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_not_equal

  subroutine parse_symbol_dimensions(self, source)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse symbol dimensions definition from string.
  !<
  !< Dimensions are returned stored into [[uom_symbol]] type.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(inout) :: self      !< The uom reference.
  type(string),         intent(inout) :: source    !< Source input string definition of symbol dimensions.
  type(string), allocatable           :: tokens(:) !< String tokens.
  integer(I_P)                        :: d(2)      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  d(1) = source%index(substring='[')
  d(2) = source%index(substring=']')
  if ((d(1)>0).and.(d(2)>0).and.(d(2)>d(1)+1)) then
    call source%split(sep='[', tokens=tokens)
    self%symbol_exponent = 0_I_P
    tokens(2) = tokens(2)%slice(1, tokens(2)%index(']')-1)
    call self%parse_symbol(source=tokens(2))
    source = tokens(1)
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse_symbol_dimensions

  subroutine parse_symbol(self, source)
  !-------------------------------------------------------------------------------------------------------------------------------
  !< Parse symbol definition from string.
  !<
  !< @note Only a single definition is handled, without considering the full symbol definition with dimensions, aliases and
  !< conversions: this is the *atomic* parser that is used to parse the full definition routinely.
  !<
  !< The subformat parsed is:
  !<
  !< `1000 * Hz-2`
  !<
  !< where the scale factor is optional as well as the exponent if it is equal to 1.
  !-------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(inout) :: self      !< The symbol.
  type(string),         intent(in)    :: source    !< String containing symbol definition.
  type(string), allocatable           :: tokens(:) !< String tokens.
  integer(I_P)                        :: e         !< Counter.
  !-------------------------------------------------------------------------------------------------------------------------------

  !-------------------------------------------------------------------------------------------------------------------------------
  call source%split(sep='*', tokens=tokens)
  if (size(tokens, dim=1)>1) then
    self%symbol_scale = cton(str=tokens(1)%chars(), knd=1._R_P)
    tokens(1) = tokens(2)
  endif
    e = tokens(1)%scan(set='-123456789')
    if (e>0) then
      self%symbol = trim(adjustl(tokens(1)%slice(1, e-1)))
      self%symbol_exponent = cton(str=tokens(1)%slice(e, tokens(1)%len()), knd=1_I_P)
    else
      self%symbol = trim(adjustl(tokens(1)%chars()))
    endif
  !-------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse_symbol

  subroutine set(self, symbol, symbol_exponent, symbol_scale, dimensions, aliases)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set symbol.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(inout)        :: self            !< The uom reference.
  character(*),         intent(in), optional :: symbol          !< Litteral symbol of the unit, e.g. "m" for metres.
  integer(I_P),         intent(in), optional :: symbol_exponent !< Exponent of the symbol, e.g. "1" for metres, namely "m1".
  real(R_P),            intent(in), optional :: symbol_scale    !< Symbol multiplicative scale factor (used only for converters).
  character(*),         intent(in), optional :: dimensions      !< Dimensions of the symbol, e.g. "length" for metres.
  type(uom_reference),  intent(in), optional :: aliases(1:)     !< Symbol aliases.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(symbol)) self%symbol = symbol
  if (present(symbol_exponent)) self%symbol_exponent = symbol_exponent
  if (present(symbol_scale)) self%symbol_scale = symbol_scale
  if (present(dimensions)) self%dimensions = dimensions
  if (present(aliases)) then
    if (associated(self%aliases)) deallocate(self%aliases)
    self%aliases_number = size(aliases, dim=1)
    allocate(self%aliases(1:self%aliases_number))
    self%aliases = aliases
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set

  ! operators
  subroutine assign_string(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference = string` assignament.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(inout) :: lhs           !< Left hand side.
  character(*),         intent(in)    :: rhs           !< Right hand side.
  type(uom_reference)                 :: parsed_symbol !< Symbol arising from string input.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call parsed_symbol%parse(source=rhs)
  if (parsed_symbol%has_symbol()) lhs = parsed_symbol
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_string

  subroutine assign_uom_reference(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference = uom_reference` assignament.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(inout) :: lhs !< Left hand side.
  type(uom_reference),  intent(in)    :: rhs !< Right hand side.
  integer(I_P)                        :: a   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (rhs%has_symbol())  then
    lhs%symbol = rhs%symbol
    lhs%symbol_exponent = rhs%symbol_exponent
    lhs%symbol_scale = rhs%symbol_scale
    if (rhs%has_dimensions()) lhs%dimensions = rhs%dimensions
    if (rhs%has_aliases()) then
      if (associated(lhs%aliases)) then
        do a=1, lhs%aliases_number
          call lhs%aliases(a)%unset
        enddo
        deallocate(lhs%aliases)
      endif
      lhs%aliases_number = size(rhs%aliases, dim=1)
      allocate(lhs%aliases(1:lhs%aliases_number))
      lhs%aliases = rhs%aliases
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_uom_reference

  function div(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference / uom_reference` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: lhs !< Left hand side.
  type(uom_reference),  intent(in) :: rhs !< Right hand side.
  type(uom_reference)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs==rhs) then
    opr = lhs
    opr%symbol_exponent = 0_I_P
    opr%symbol_scale = 1._R_P
  elseif (lhs%is_compatible(other=rhs)) then
    opr = lhs
    opr%symbol_exponent = lhs%symbol_exponent - rhs%symbol_exponent
    opr%symbol_scale = lhs%symbol_scale / rhs%get_alias_scale(alias_symbol=lhs%symbol)
  endif
  ! aliases must be eliminated
  call opr%unset(only_aliases=.true.)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction div

  function mul(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference * uom_reference` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: lhs !< Left hand side.
  type(uom_reference),  intent(in) :: rhs !< Right hand side.
  type(uom_reference)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs==rhs) then
    opr = lhs
    opr%symbol_exponent = opr%symbol_exponent*2_I_P
    opr%symbol_scale = lhs%symbol_scale ** 2
  elseif (lhs%is_compatible(other=rhs)) then
    opr = lhs
    opr%symbol_exponent = lhs%symbol_exponent + rhs%symbol_exponent
    opr%symbol_scale = lhs%symbol_scale * rhs%get_alias_scale(alias_symbol=lhs%symbol)
  endif
  ! aliases must be eliminated
  call opr%unset(only_aliases=.true.)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction mul

  function pow_I8P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference ** integer(I8P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: lhs !< Left hand side.
  integer(I8P),         intent(in) :: rhs !< Right hand side.
  type(uom_reference)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%has_symbol()) then
    opr = lhs
    opr%symbol_exponent = lhs%symbol_exponent * rhs
    opr%symbol_scale = lhs%symbol_scale ** rhs
  endif
  ! aliases must be eliminated
  call opr%unset(only_aliases=.true.)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I8P

  function pow_I4P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference ** integer(I4P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: lhs !< Left hand side.
  integer(I4P),         intent(in) :: rhs !< Right hand side.
  type(uom_reference)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%has_symbol()) then
    opr = lhs
    opr%symbol_exponent = lhs%symbol_exponent * rhs
    opr%symbol_scale = lhs%symbol_scale ** rhs
  endif
  ! aliases must be eliminated
  call opr%unset(only_aliases=.true.)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I4P

  function pow_I2P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference ** integer(I2P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: lhs !< Left hand side.
  integer(I2P),         intent(in) :: rhs !< Right hand side.
  type(uom_reference)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%has_symbol()) then
    opr = lhs
    opr%symbol_exponent = lhs%symbol_exponent * rhs
    opr%symbol_scale = lhs%symbol_scale ** rhs
  endif
  ! aliases must be eliminated
  call opr%unset(only_aliases=.true.)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I2P

  function pow_I1P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom_reference ** integer(I1P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom_reference), intent(in) :: lhs !< Left hand side.
  integer(I1P),         intent(in) :: rhs !< Right hand side.
  type(uom_reference)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs%has_symbol()) then
    opr = lhs
    opr%symbol_exponent = lhs%symbol_exponent * rhs
    opr%symbol_scale = lhs%symbol_scale ** rhs
  endif
  ! aliases must be eliminated
  call opr%unset(only_aliases=.true.)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I1P
endmodule fury_uom_reference
