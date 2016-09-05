!< FURY definition of unit of measure class.
module fury_uom
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY definition of unit of measure class.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : stderr => error_unit
use fury_prefixes
use fury_uom_reference
use fury_uom_symbol
use penf
use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: uom
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: uom
  !< Unit of measure (UOM) class.
  !<
  !< The string format definition of a valid FURY unit is as following:
  !<
  !< `kg [mass].m-1 = metre-1 = meter-1 [length-1].s-2 = seconds-2 = sec-2 [time-2](Pa[pressure]){pascal}`
  !< `km = 1000 * m [length].h-1 = 3600 s-1 [time-1](km/h[speed]){km/h}`
  !<
  !< where
  !<
  !<+ the terms `[...]` define the *dimension* of each reference and are optional (the white spaces are ignored); moreover the
  !<  exponents of dimensions can be omitted: in this case they are inferred from the symbol reference exponents; in the case
  !<  they are explicitely written they must match the corresponding symbols ones or an error is raised;
  !<+ the term `(...)` defines a unit *alias* that is optional and must come always after unit reference definition;
  !<+ the term `{...}` defines the unit name that is optional and must be always the last term.
  !<
  !< Other valid string inputs of the same above *pressure* unit are:
  !<
  !< `kg.m-1.s-2` a unit without specified dimensions, alias and name;
  !< `kg.m-1.s-2(Pa)` a unit without specified dimensions and name, but with an alias without alias dimension;
  !< `kg.m-1.s-2(Pa[pressure])` a unit without specified dimensions and name, but with an alias with alias dimension;
  !< `kg.m-1.s-2{pascal}` a unit without specified dimensions and alias, but with a name;
  !< `kg.m-1.s-2(Pa){pascal}` a unit without specified dimensions, but with an alias without alias dimension and a name;
  !<
  !< @note It is better to avoid to uncomplete list of dimensions for references: define all dimensions for all references
  !< or avoid to define dimensions at all.
  type(uom_reference), allocatable, private :: references(:)           !< Reference units of the unit.
  type(uom_reference), allocatable, private :: alias                   !< Alias of the unit, e.g Pa (kg.m-1.s-2) for Pascal.
  integer(I_P),                     private :: references_number=0_I_P !< References number.
  character(len=:), allocatable             :: name                    !< Unit name.
  contains
    ! public methods
    procedure, pass(self) :: get_conversion_factor !< Get the scale factor from an alias.
    procedure, pass(self) :: get_main_aliases      !< Return the main aliases.
    procedure, pass(self) :: get_main_dimensions   !< Return the main dimensions.
    procedure, pass(self) :: get_main_reference    !< Return the main reference.
    procedure, pass(self) :: get_main_symbol       !< Return the main symbol.
    procedure, pass(self) :: has_reference         !< Check if the unit has a refence unit.
    procedure, pass(self) :: is_defined            !< Check if the unit is defined.
    procedure, pass(self) :: prefixed_unit         !< Return prefixed unit.
    procedure, pass(self) :: set                   !< Set the unit.
    procedure, pass(self) :: stringify             !< Return a string representaion of the unit.
    procedure, pass(self) :: unset                 !< unset the unit.
    ! public generic names
    generic :: assignment(=) => assign_string, &
                                assign_uom             !< Overloading `=` operator.
    generic :: operator(+) => add                      !< Overloading `+` operator.
    generic :: operator(/) => div                      !< Overloading `/` operator.
    generic :: operator(*) => mul                      !< Overloading `*` operator.
    generic :: operator(-) => sub                      !< Overloading `-` operator.
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
    procedure, pass(self), private :: add_reference            !< Add a refence unit to unit.
    procedure, pass(self), private :: has_alias                !< Check if the unit has an alias.
    procedure, pass(self), private :: has_name                 !< Check if the unit has a name.
    procedure, pass(self), private :: has_reference_compatible !< Check if the unit has a compatible refence unit.
    procedure, pass(self), private :: is_compatible            !< Check if unit is compatible with another one.
    procedure, pass(self), private :: is_equal                 !< Check if unit is equal with another one.
    procedure, pass(self), private :: is_not_equal             !< Check if unit is not equal with another one.
    procedure, pass(self), private :: parse                    !< Parse unit definition from an input string.
    procedure, pass(self), private :: parse_alias              !< Parse unit alias from an input string.
    procedure, pass(self), private :: parse_name               !< Parse unit name from an input string.
    procedure, pass(self), private :: parse_references         !< Parse refence units from an input string.
    procedure, pass(self), private :: update_references_number !< Update refence units number counter.
    ! operators
    procedure, pass(lhs), private :: assign_string !< `uom = string` assignament.
    procedure, pass(lhs), private :: assign_uom    !< `uom = uom` assignament.
    procedure, pass(lhs), private :: add           !< `uom + uom` operator.
    procedure, pass(lhs), private :: div           !< `uom / uom` operator.
    procedure, pass(lhs), private :: mul           !< `uom * uom` operator.
    procedure, pass(lhs), private :: sub           !< `uom - uom` operator.
    procedure, pass(lhs), private :: pow_R16P      !< `uom ** real(R16P)` operator.
    procedure, pass(lhs), private :: pow_R8P       !< `uom ** real(R8P)` operator.
    procedure, pass(lhs), private :: pow_R4P       !< `uom ** real(R4P)` operator.
    procedure, pass(lhs), private :: pow_I8P       !< `uom ** integer(I8P)` operator.
    procedure, pass(lhs), private :: pow_I4P       !< `uom ** integer(I4P)` operator.
    procedure, pass(lhs), private :: pow_I2P       !< `uom ** integer(I2P)` operator.
    procedure, pass(lhs), private :: pow_I1P       !< `uom ** integer(I1P)` operator.
endtype uom

interface uom
  !< Ovearloading [[uom]] name with a set of creator functions.
  module procedure creator_from_string, creator_from_other_unit
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! private non type bound procedures
  function creator_from_string(source, alias, name) result(unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Create an instance of unit from an input source string..
  !---------------------------------------------------------------------------------------------------------------------------------
  character(*),        intent(in)           :: source !< Source input string definition of the unit.
  type(uom_reference), intent(in), optional :: alias  !< Alias of the unit, e.g Pa (kg.m-1.s-2) for Pascal [pressure].
  character(*),        intent(in), optional :: name   !< Unit name.
  type(uom)                                 :: unit   !< The unit.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call unit%parse(source=source)
  call unit%set(alias=alias, name=name)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction creator_from_string
!
  function creator_from_other_unit(source, alias, name) result(unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Create an instance of unit from another unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(uom),           intent(in)           :: source !< Source input unit.
  type(uom_reference), intent(in), optional :: alias  !< Alias of the unit, e.g Pa (kg.m-1.s-2) for Pascal [pressure].
  character(*),        intent(in), optional :: name   !< Unit name.
  type(uom)                                 :: unit   !< The unit.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  unit = source
  call unit%set(alias=alias, name=name)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction creator_from_other_unit

  subroutine remove_reference(references, id)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Remove a reference from a list of rerefences.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(uom_reference), intent(inout), allocatable :: references(:)     !< References list.
  integer(I_P),        intent(in)                 :: id                !< Index of reference to remove.
  type(uom_reference), allocatable                :: references_tmp(:) !< References list temporary copy.
  integer(I_P)                                    :: references_number !< References number.
  integer(I_P)                                    :: i                 !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  references_number = size(references, dim=1)
  allocate(references_tmp(1:references_number-1))
  if (id==1) then
    do i=2, references_number
      references_tmp(i-1) = references(i)
    enddo
  elseif (id==references_number )  then
    do i=1, references_number-1
      references_tmp(i) = references(i)
    enddo
  else
    do i=1, id-1
      references_tmp(i) = references(i)
    enddo
    do i=id, references_number-1
      references_tmp(i) = references(i+1)
    enddo
  endif
  call move_alloc(from=references_tmp, to=references)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine remove_reference

  ! public methods
  pure function get_conversion_factor(self, alias) result(factor)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get the scale factor from an alias.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in) :: self       !< The uom.
  type(uom),  intent(in) :: alias      !< Alias uom queried.
  real(R_P)              :: factor     !< Symbol scale factor.
  type(uom_symbol)       :: s_main     !< Main self reference symbol.
  type(uom_symbol)       :: a_main     !< Main alias reference symbol.
  type(uom_symbol)       :: conversion !< Conversion uom symbol.
  integer(I_P)           :: r          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  factor = 1._R_P
  if (self%is_defined().and.alias%is_defined().and.self%references_number==alias%references_number) then
    do r=1, self%references_number
      if (alias%references(r).compatible.self%references(r)) then
        s_main = self%references(r)%get_main_symbol()
        a_main = alias%references(r)%get_first_compatible_alias(self%references(r))
        conversion = s_main%to(a_main)
        factor = factor * conversion%get_factor()
      endif
    enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction get_conversion_factor

  function get_main_symbol(self) result(symbol)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the main symbol, i.e. `self%alias%aliases(1)%symbol_` if `self%alias` is defined or
  !< `self%references(1)%aliases(1)%symbol_` if `self` has only 1 [[uom_reference]].
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in)  :: self              !< The uom.
  type(string)            :: symbol            !< Main alias symbol.
  type(uom_reference)     :: main_reference    !< Main uom reference.
  type(uom_symbol)        :: main_alias_symbol !< Main alias symbol.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  main_reference = self%get_main_reference()
  if (main_reference%is_defined()) then
    main_alias_symbol = main_reference%get_main_symbol()
    if (main_alias_symbol%is_defined()) symbol = main_alias_symbol%get_symbol()
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction get_main_symbol

  pure function get_main_aliases(self) result(aliases)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the main aliases, i.e. `self%alias%aliases(1)%aliases(:)%symbol_` if `self%alias` is defined or
  !< `self%references(1)%aliases(1)%aliases(:)%symbol_` is `self` has only 1 [[uom_reference]].
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in)    :: self       !< The uom reference.
  type(string), allocatable :: aliases(:) !< Main alias aliases.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_defined()) then
    if (self%has_alias()) then
      aliases = self%alias%get_aliases()
    elseif (self%references_number==1) then
      aliases = self%references(1)%get_aliases()
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction get_main_aliases

  pure function get_main_dimensions(self) result(dimensions)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the main dimensions, i.e. `self%alias%dimensions%symbol_` if `self%alias` is defined or
  !< `self%references(1)%dimensions%symbol_` if `self` has only 1 [[uom_reference]].
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in)  :: self       !< The uom reference.
  type(string)            :: dimensions !< Main alias dimensions.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_defined()) then
    if (self%has_alias()) then
      dimensions = self%alias%dimensionality()
    elseif (self%references_number==1) then
      dimensions = self%references(1)%dimensionality()
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction get_main_dimensions

  pure function get_main_reference(self) result(reference)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the main symbol, i.e. `self%alias` if `self%alias` is defined or `self%references(1)` if `self` has only 1
  !< [[uom_reference]].
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in) :: self      !< The uom.
  type(uom_reference)    :: reference !< Main uom reference.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_defined()) then
    if (self%has_alias()) then
      reference = self%alias
    elseif (self%references_number==1) then
      reference = self%references(1)
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction get_main_reference

  function prefixed_unit(self, prefix)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return prefixed unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),     intent(in)    :: self                !< The unit.
  type(prefixes), intent(in)    :: prefix              !< Prefixes data.
  type(uom)                     :: prefixed_unit       !< Prefixed unit.
  type(uom_reference)           :: prefixed_reference  !< Prefixed unit reference.
  type(uom_symbol), allocatable :: prefixed_aliases(:) !< Prefixed unit symbol aliases.
  type(uom_symbol)              :: prefixed_dimensions !< Prefixed unit symbol dimensions.
  type(string)                  :: symbol              !< Base symbol to be prefixed.
  integer(I_P)                  :: aliases_number      !< Counter.
  integer(I_P)                  :: a                   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_defined().and.prefix%is_defined()) then
    prefixed_unit = self
    symbol = prefixed_unit%get_main_symbol()
    call prefixed_dimensions%set(symbol_=prefixed_unit%get_main_dimensions()//'')
    aliases_number = size(prefix%aliases, dim=1)
    allocate(prefixed_aliases(1:aliases_number+1))
    do a=1, aliases_number
      call prefixed_aliases(a)%set(symbol_=prefix%aliases(a)//symbol)
    enddo
    call prefixed_aliases(aliases_number+1)%set(symbol_=symbol%chars(), factor_=prefix%factor)
    call prefixed_reference%set(aliases=prefixed_aliases, dimensions=prefixed_dimensions)
    call prefixed_unit%set(references=[prefixed_reference])
    if (prefixed_unit%has_name()) call prefixed_unit%set(name=prefix%aliases(1)//prefixed_unit%name)
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction prefixed_unit

  elemental function has_reference(self, reference)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the unit has a reference unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),          intent(in) :: self          !< The unit.
  type(uom_reference), intent(in) :: reference     !< Reference unit to check the presence of.
  logical                         :: has_reference !< reference unit presence status.
  integer(I_P)                    :: r             !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  has_reference = .false.
  if (self%is_defined()) then
    do r=1, self%references_number
      has_reference = self%references(r) == reference
      if (has_reference) exit
    enddo
    if (.not.has_reference) then
      if (self%has_alias()) has_reference = self%alias == reference
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction has_reference

  elemental function is_defined(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the unit has been defined, namely it has defined references.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in) :: self       !< The unit.
  logical                :: is_defined !< Definition status.
  integer                :: r          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_defined = allocated(self%references) ! possible GNU bug
  ! is_defined = (self%references_number>0)
  if (is_defined) then
    do r=1, self%references_number
      is_defined = self%references(r)%is_defined()
      if (.not.is_defined) exit
    enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_defined

  pure subroutine set(self, references, alias, name)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set the unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),          intent(inout)         :: self           !< The unit.
  type(uom_reference), intent(in),  optional :: references(1:) !< Unit references of the unit.
  type(uom_reference), intent(in),  optional :: alias          !< Alias of the unit, e.g Pa (kg.m-1.s-2) for Pascal [pressure].
  character(*),        intent(in),  optional :: name           !< Unit name.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(references)) then
    if (allocated(self%references)) deallocate(self%references)
    allocate(self%references(1:size(references, dim=1)))
    self%references = references
  endif
  call self%update_references_number()
  if (present(alias)) then
    if (.not.allocated(self%alias)) allocate(self%alias)
    self%alias = alias
  endif
  if (present(name)) self%name = name
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set

  pure function stringify(self, with_dimensions, with_aliases, protect_aliases, with_name, compact_reals) result(raw)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a string representation of the unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in)           :: self            !< The unit.
  logical,    intent(in), optional :: with_dimensions !< Flag to activate dimensions printing.
  logical,    intent(in), optional :: with_aliases    !< Flag to activate aliases printing.
  logical,    intent(in), optional :: protect_aliases !< Flag to activate aliases printing in protected mode.
  logical,    intent(in), optional :: with_name       !< Flag to activate name printing.
  logical,    intent(in), optional :: compact_reals   !< Flag to activate real numbers compacting.
  character(len=:), allocatable    :: raw             !< Raw characters data.
  character(len=:), allocatable    :: dimensions      !< Dimensions in raw characters data.
  integer(I_P)                     :: s               !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_defined()) then
    raw = ''
    do s=1, self%references_number
      raw = raw//'.'//self%references(s)%stringify(with_aliases=with_aliases,       &
                                                   protect_aliases=protect_aliases, &
                                                   compact_reals=compact_reals)
    enddo
    raw = raw(2:)
    if (present(with_dimensions)) then
      if (with_dimensions) then
        dimensions = ''
        do s=1, self%references_number
          dimensions = dimensions//self%references(s)%dimensionality()//'.'
        enddo
        if (dimensions(1:1)=='.'.and.len(dimensions)>1) dimensions = dimensions(2:)
        if (dimensions(len(dimensions):len(dimensions))=='.') dimensions = dimensions(:len(dimensions)-1)
        raw = raw//' ['//dimensions//']'
      endif
    endif
    if (present(with_aliases)) then
      if (with_aliases.and.self%has_alias()) then
        raw = raw//' ('//self%alias%stringify(with_dimensions=with_dimensions, compact_reals=compact_reals)//')'
      endif
    endif
    if (present(with_name)) then
      if (with_name.and.self%has_name()) then
        raw = raw//' {'//self%name//'}'
      endif
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction stringify

  elemental subroutine unset(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Unset the unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(inout) :: self !< The unit.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%references)) deallocate(self%references)
  if (allocated(self%alias)) deallocate(self%alias)
  self%references_number = 0_I_P
  if (allocated(self%name)) deallocate(self%name)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine unset

  ! private methods
  pure subroutine add_reference(self, reference)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add reference unit to unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),          intent(inout) :: self          !< The unit.
  type(uom_reference), intent(in)    :: reference     !< Unit reference to be added.
  type(uom_reference), allocatable   :: references(:) !< Reference unit(s) temporary array.
  integer(I_P)                       :: r             !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_defined().and.(.not.self%has_reference(reference=reference))) then
    allocate(references(self%references_number+1))
    ! references(1:self%references_number) = self%references
    do r=1, self%references_number
      references(r) = self%references(r)
    enddo
    references(self%references_number+1) = reference
    call move_alloc(from=references, to=self%references)
    self%references_number = self%references_number + 1_I_P
  else
    allocate(self%references(1))
    self%references(1) = reference
    self%references_number = 1_I_P
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_reference

  elemental function has_alias(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the unit has an alias.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in) :: self      !< The unit.
  logical                :: has_alias !< Alias presence status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  has_alias = allocated(self%alias)
  if (has_alias) has_alias = self%alias%is_defined()
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction has_alias

  elemental function has_name(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the unit has a name.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in) :: self     !< The unit.
  logical                :: has_name !< Name presence status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  has_name = allocated(self%name)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction has_name

  elemental function has_reference_compatible(self, reference)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the unit has a compatible reference unit.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),          intent(in) :: self                     !< The unit.
  type(uom_reference), intent(in) :: reference                !< Reference unit to check the presence of.
  logical                         :: has_reference_compatible !< reference unit presence status.
  integer(I_P)                    :: r                        !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  has_reference_compatible = .false.
  if (self%is_defined()) then
    do r=1, self%references_number
      has_reference_compatible = self%references(r).compatible.reference
      if (has_reference_compatible) exit
    enddo
    if (.not.has_reference_compatible) then
      if (self%has_alias()) has_reference_compatible = self%alias.compatible.reference
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction has_reference_compatible

  elemental function is_compatible(self, other)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if unit is compatible with another one.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in) :: self          !< The unit.
  class(uom), intent(in) :: other         !< The other unit.
  logical                :: is_compatible !< Compatibility check result.
  integer(I_P)           :: r             !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_compatible = .false.
  if (self%is_defined().and.other%is_defined()) then
    is_compatible = (self%references_number==other%references_number)
    if (is_compatible) then
      do r=1, self%references_number
        is_compatible = other%has_reference_compatible(reference=self%references(r))
        if (.not.is_compatible) exit
      enddo
    endif
    if (.not.is_compatible) then
      ! compare against alias
      if (self%has_alias().and.other%references_number==1) then
        is_compatible = other%references(1).compatible.self%alias
      elseif (other%has_alias().and.self%references_number==1) then
        is_compatible = self%references(1).compatible.other%alias
      endif
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_compatible

  elemental function is_equal(self, other)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if unit is equal (has the same references units) with another one.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in) :: self     !< The unit.
  class(uom), intent(in) :: other    !< The other unit.
  logical                :: is_equal !< Equality check result.
  integer(I_P)           :: r        !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_equal = .false.
  if (self%is_defined().and.other%is_defined()) then
    is_equal = (self%references_number==other%references_number)
    if (is_equal) then
      do r=1, self%references_number
        is_equal = other%has_reference(reference=self%references(r))
        if (.not.is_equal) exit
      enddo
    endif
    if (.not.is_equal) then
      ! compare against alias
      if (self%has_alias().and.other%references_number==1) then
        is_equal = other%references(1) == self%alias
      elseif (other%has_alias().and.self%references_number==1) then
        is_equal = self%references(1) == other%alias
      endif
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_equal

  elemental function is_not_equal(self, other)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if unit is not equal (has not the same references units) with another one.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in) :: self         !< The unit.
  class(uom), intent(in) :: other        !< The other unit.
  logical                :: is_not_equal !< Disequality check result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_not_equal = .not.self%is_equal(other=other)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_not_equal

  subroutine parse(self, source)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse unit definition form an input string.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),   intent(inout) :: self       !< The unit.
  character(*), intent(in)    :: source     !< Input source string.
  type(string)                :: source_str !< Source input stringified.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%unset
  source_str = trim(adjustl(source))
  call self%parse_name(source=source_str)
  call self%parse_alias(source=source_str)
  call self%parse_references(source=source_str)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse

  subroutine parse_alias(self, source)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse unit alias form an input string and return also the source string without the alias data.
  !<
  !< @note It is assumed that the optional unit name has been already parsed and trimmed out form the input string.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),   intent(inout) :: self      !< The unit.
  type(string), intent(inout) :: source    !< Input source string.
  type(string), allocatable   :: tokens(:) !< String tokens.
  integer(I_P)                :: n1        !< Counter.
  integer(I_P)                :: n2        !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n1 = source%count('(')
  n2 = source%count(')')
  if (n1==1.and.n2==1) then
    call source%split(sep='(', tokens=tokens)
    source = tokens(1)
    tokens(2) = tokens(2)%replace(old=')', new='')
    if (.not.allocated(self%alias)) allocate(self%alias)
    call self%alias%parse(source=tokens(2)%chars())
  elseif (n1>1.or.n2>1) then
    write(stderr, '(A)')'error: input source string "'//source//'" has bad alias specifier for the unit!'
    stop
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse_alias

  subroutine parse_name(self, source)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse unit name form an input string and return also the source string without the name data.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),   intent(inout) :: self      !< The unit.
  type(string), intent(inout) :: source    !< Input source string.
  type(string), allocatable   :: tokens(:) !< String tokens.
  integer(I_P)                :: n1        !< Counter.
  integer(I_P)                :: n2        !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n1 = source%count('{')
  n2 = source%count('}')
  if (n1==1.and.n2==1) then
    call source%split(sep='{', tokens=tokens)
    source = tokens(1)
    tokens(2) = tokens(2)%replace(old='}', new='')
    self%name = tokens(2)%chars()
  elseif (n1>1.or.n2>1) then
    write(stderr, '(A)')'error: input source string "'//source//'" has bad name specifier for the unit!'
    stop
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse_name

  subroutine parse_references(self, source)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse references units form an input string.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),   intent(inout) :: self          !< The unit.
  type(string), intent(in)    :: source        !< Input source string.
  type(string)                :: buffer        !< String buffer.
  type(string)                :: alias         !< String buffer.
  type(string)                :: aliases       !< String buffer.
  integer(I_P)                :: istart        !< Starting index of alias tag inside the string.
  integer(I_P)                :: iend          !< Ending index of alias tag inside the string.
  type(string), allocatable   :: tokens(:)     !< String tokens.
  integer(I_P)                :: tokens_number !< Tokens number.
  integer(I_P)                :: t             !< Counter.
  integer(I_P)                :: offset        !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  buffer = source
  if (buffer%count('<')>0) then
    ! aliases are protected because there are alias formulas with "." conflicting with "." symbols separators
    aliases = repeat(' ', buffer%len())
    do while(buffer%count('<')>0)
      alias = buffer%search(tag_start='<', tag_end='>', istart=istart, iend=iend)
      buffer = buffer%replace(old=alias%chars(), new=repeat(' ', alias%len()))
      aliases = aliases%slice(1, istart-1)//alias//aliases%slice(iend+1, aliases%len())
    enddo
    if (aliases/='') then
      call buffer%split(tokens=tokens, sep='.')
      tokens_number = size(tokens, dim=1)
      offset = 0
      do t=1, tokens_number
        if (t>1) offset = offset + tokens(t-1)%len() + 1
        do while(aliases%count('<')>0)
          alias = aliases%search(tag_start='<', tag_end='>', istart=istart, iend=iend)
          if (iend<=tokens(t)%len()+offset) then
            tokens(t) = tokens(t)%slice(1, istart-offset-1)//alias//tokens(t)%slice(iend-offset+1, tokens(t)%len())
            aliases = aliases%replace(old=alias%chars(), new=repeat(' ', alias%len()))
          else
            exit
          endif
        enddo
      enddo
      do t=1, tokens_number
        tokens(t) = tokens(t)%replace(old='<', new='')
        tokens(t) = tokens(t)%replace(old='>', new='')
      enddo
    endif
  else
    ! aliases are not protected, symbols can be safely tokenized by "."
    call buffer%split(tokens=tokens, sep='.')
  endif
  tokens_number = size(tokens, dim=1)
  if (allocated(self%references)) deallocate(self%references)
  allocate(self%references(1:tokens_number))
  do t=1, tokens_number
    call self%references(t)%parse(source=tokens(t)%chars())
  enddo
  call self%update_references_number()
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse_references

  pure subroutine update_references_number(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Update references number counter.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(inout) :: self !< The unit.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%references_number = 0_I_P
  if (self%is_defined()) self%references_number = size(self%references, dim=1)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine update_references_number

  ! operators
  subroutine assign_string(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom = string` assignament.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),   intent(inout) :: lhs         !< Left hand side.
  character(*), intent(in)    :: rhs         !< Right hand side.
  type(uom)                   :: parsed_unit !< Unit arising from string input.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call parsed_unit%parse(source=rhs)
  if (.not.lhs%is_defined())  then
    lhs = parsed_unit
  else
    if (.not.lhs == parsed_unit) then
      write(stderr, "(A)") 'error: cannot convert from "'//lhs%stringify(with_dimensions=.true.)//'" to "'//&
                           parsed_unit%stringify(with_dimensions=.true.)//'"'
      stop
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_string

  subroutine assign_uom(lhs, rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom = uom` assignament.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(inout) :: lhs !< Left hand side.
  class(uom), intent(in)    :: rhs !< Right hand side.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (rhs%is_defined())  then
    if (.not.lhs%is_defined())  then
      if (allocated(lhs%references)) deallocate(lhs%references)
      ! lhs%references = rhs%references ! possible GNU bug
      allocate(lhs%references, source=rhs%references)
      lhs%references_number = rhs%references_number
      if (allocated(rhs%alias)) then
        if (.not.allocated(lhs%alias)) allocate(lhs%alias)
        lhs%alias = rhs%alias
      endif
      if (allocated(rhs%name)) lhs%name = rhs%name
    else
      if (.not.lhs==rhs) then
        write(stderr, "(A)") 'error: cannot convert from "'//lhs%stringify(with_dimensions=.true.)//'" to "'//&
                             rhs%stringify(with_dimensions=.true.)//'"'
        stop
      endif
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_uom

  function add(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom + uom` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in) :: lhs !< Left hand side.
  class(uom), intent(in) :: rhs !< Right hand side.
  type(uom)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs == rhs) then
    opr = lhs
  else
    write(stderr, "(A)") 'error: cannot convert from "'//lhs%stringify(with_dimensions=.true.)//'" to "'//&
                         rhs%stringify(with_dimensions=.true.)//'"'
    stop
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction add

  function div(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom / uom` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in)           :: lhs               !< Left hand side.
  class(uom), intent(in)           :: rhs               !< Right hand side.
  type(uom)                        :: opr               !< Operator result.
  type(uom_reference), allocatable :: lhs_references(:) !< Left hand side references.
  type(uom_reference), allocatable :: rhs_references(:) !< Right hand side references.
  integer(I_P)                     :: ls                !< Counter.
  integer(I_P)                     :: rs                !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  if (lhs%is_defined().and.rhs%is_defined()) then
    ! lhs_references = lhs%references ! possible GNU bug
    ! rhs_references = rhs%references ! possible GNU bug
    allocate(lhs_references, source=lhs%references)
    allocate(rhs_references, source=rhs%references)
    do ls=1, size(lhs_references, dim=1)
      rs = 1
      remaining_rhs_references: do
        if (lhs_references(ls).compatible.rhs_references(rs)) then
          lhs_references(ls) = lhs_references(ls) / rhs_references(rs)
          ! pop up current reference from rhs references
          if (size(rhs_references, dim=1)>1) then
            call remove_reference(references=rhs_references, id=rs)
            rs = 1
          else
            deallocate(rhs_references)
          endif
        else
          ! check the next rhs references
          rs = rs + 1
        endif
        if (rs>=size(rhs_references, dim=1).or.(.not.allocated(rhs_references))) exit remaining_rhs_references
      enddo remaining_rhs_references
    enddo
    opr%references = lhs_references
    if (allocated(rhs_references)) then
      ! there are still rhs references not compatible with lhs ones that must be added
      do rs=1, size(rhs_references, dim=1)
        call opr%add_reference(reference=rhs_references(rs)**(-1))
      enddo
    endif
    if (lhs%has_name().and.rhs%has_name()) then
      opr%name = lhs%name//'/'//rhs%name
    endif
  endif
  call opr%update_references_number()
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction div

  function mul(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom * uom` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in)           :: lhs               !< Left hand side.
  class(uom), intent(in)           :: rhs               !< Right hand side.
  type(uom)                        :: opr               !< Operator result.
  type(uom_reference), allocatable :: lhs_references(:) !< Left hand side references.
  type(uom_reference), allocatable :: rhs_references(:) !< Right hand side references.
  integer(I_P)                     :: ls                !< Counter.
  integer(I_P)                     :: rs                !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  if (lhs%is_defined().and.rhs%is_defined()) then
    ! lhs_references = lhs%references ! possible GNU bug
    ! rhs_references = rhs%references ! possible GNU bug
    allocate(lhs_references, source=lhs%references)
    allocate(rhs_references, source=rhs%references)
    do ls=1, size(lhs_references, dim=1)
      rs = 1
      remaining_rhs_references: do
        if (lhs_references(ls).compatible.rhs_references(rs)) then
          lhs_references(ls) = lhs_references(ls) * rhs_references(rs)
          ! pop up current reference from rhs references
          if (size(rhs_references, dim=1)>1) then
            call remove_reference(references=rhs_references, id=rs)
            rs = 1
          else
            deallocate(rhs_references)
          endif
        else
          ! check the next rhs references
          rs = rs + 1
        endif
        if (rs>=size(rhs_references, dim=1).or.(.not.allocated(rhs_references))) exit remaining_rhs_references
      enddo remaining_rhs_references
    enddo
    opr%references = lhs_references
    if (allocated(rhs_references)) then
      ! there are still rhs references not compatible with lhs ones that must be added
      do rs=1, size(rhs_references, dim=1)
        call opr%add_reference(reference=rhs_references(rs))
      enddo
    endif
    if (lhs%has_name().and.rhs%has_name()) then
      opr%name = lhs%name//'*'//rhs%name
    endif
  endif
  call opr%update_references_number()
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction mul

  function sub(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom - uom` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in) :: lhs !< Left hand side.
  class(uom), intent(in) :: rhs !< Right hand side.
  type(uom)              :: opr !< Operator result.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (lhs==rhs) then
    opr = lhs
  else
    write(stderr, "(A)") 'error: cannot convert from "'//lhs%stringify(with_dimensions=.true.)//'" to "'//&
                         rhs%stringify(with_dimensions=.true.)//'"'
    stop
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction sub

  function pow_R16P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom ** real(R16P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in) :: lhs !< Left hand side.
  real(R16P), intent(in) :: rhs !< Right hand side.
  type(uom)              :: opr !< Operator result.
  integer(I_P)           :: r   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  if (opr%is_defined()) then
    do r=1, size(opr%references, dim=1)
      opr%references(r) = opr%references(r) ** rhs
    enddo
    if (opr%has_alias()) then
      if (opr%alias%is_defined()) opr%alias = opr%alias ** rhs
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_R16P

  function pow_R8P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom ** real(R8P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in) :: lhs !< Left hand side.
  real(R8P),  intent(in) :: rhs !< Right hand side.
  type(uom)              :: opr !< Operator result.
  integer(I_P)           :: r   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  if (opr%is_defined()) then
    do r=1, size(opr%references, dim=1)
      opr%references(r) = opr%references(r) ** rhs
    enddo
    if (opr%has_alias()) then
      if (opr%alias%is_defined()) opr%alias = opr%alias ** rhs
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_R8P

  function pow_R4P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom ** real(R4P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom), intent(in) :: lhs !< Left hand side.
  real(R4P),  intent(in) :: rhs !< Right hand side.
  type(uom)              :: opr !< Operator result.
  integer(I_P)           :: r   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  if (opr%is_defined()) then
    do r=1, size(opr%references, dim=1)
      opr%references(r) = opr%references(r) ** rhs
    enddo
    if (opr%has_alias()) then
      if (opr%alias%is_defined()) opr%alias = opr%alias ** rhs
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_R4P

  function pow_I8P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom ** integer(I8P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),   intent(in) :: lhs !< Left hand side.
  integer(I8P), intent(in) :: rhs !< Right hand side.
  type(uom)                :: opr !< Operator result.
  integer(I_P)             :: r   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  if (opr%is_defined()) then
    do r=1, size(opr%references, dim=1)
      opr%references(r) = opr%references(r) ** rhs
    enddo
    if (opr%has_alias()) then
      if (opr%alias%is_defined()) opr%alias = opr%alias ** rhs
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I8P

  function pow_I4P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom ** integer(I4P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),   intent(in) :: lhs !< Left hand side.
  integer(I4P), intent(in) :: rhs !< Right hand side.
  type(uom)                :: opr !< Operator result.
  integer(I_P)             :: r   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  if (opr%is_defined()) then
    do r=1, size(opr%references, dim=1)
      opr%references(r) = opr%references(r) ** rhs
    enddo
    if (opr%has_alias()) then
      if (opr%alias%is_defined()) opr%alias = opr%alias ** rhs
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I4P

  function pow_I2P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom ** integer(I2P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),   intent(in) :: lhs !< Left hand side.
  integer(I2P), intent(in) :: rhs !< Right hand side.
  type(uom)                :: opr !< Operator result.
  integer(I_P)             :: r   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  if (opr%is_defined()) then
    do r=1, size(opr%references, dim=1)
      opr%references(r) = opr%references(r) ** rhs
    enddo
    if (opr%has_alias()) then
      if (opr%alias%is_defined()) opr%alias = opr%alias ** rhs
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I2P

  function pow_I1P(lhs, rhs) result(opr)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< `uom ** integer(I1P)` operator.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(uom),   intent(in) :: lhs !< Left hand side.
  integer(I1P), intent(in) :: rhs !< Right hand side.
  type(uom)                :: opr !< Operator result.
  integer(I_P)             :: r   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  opr = lhs
  if (opr%is_defined()) then
    do r=1, size(opr%references, dim=1)
      opr%references(r) = opr%references(r) ** rhs
    enddo
    if (opr%has_alias()) then
      if (opr%alias%is_defined()) opr%alias = opr%alias ** rhs
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pow_I1P
endmodule fury_uom
