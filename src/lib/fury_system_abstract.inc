!< FURY implementation of abstract units system with generic kind.

implicit none
private
public :: system_abstract

type, abstract :: system_abstract
  !< Abstract units system class.
  character(len=:),    allocatable :: acronym                 !< Units system acronym, e.g. "SI" for the International System.
  type(qreal),         allocatable :: constants(:)            !< Defined constants.
  integer(I_P)                     :: constants_number=0_I_P  !< Number of constants.
  type(uom),           allocatable :: units(:)                !< Defined units.
  integer(I_P)                     :: units_number=0_I_P      !< Number of units.
  type(uom_reference), allocatable :: prefixes(:)             !< Prefixes.
  integer(I_P)                     :: prefixes_number=0_I_P   !< Number of prefixes.
  contains
    ! public deferred methods
    procedure(initialize_interface), pass(self), deferred :: initialize !< Initialize the units system.
    ! public methods
    generic               :: add_constant => add_constant_qreal     !< Add a new constant.
    generic               :: add_prefix => add_prefix_string, &
                                           add_prefix_uom_reference !< Add a new prefix.
    generic               :: add_unit => add_unit_string, &
                                         add_unit_uom               !< Add a new unit.
    procedure, pass(self) :: const                                  !< Return an instance of the queried constant (if defined).
    procedure, pass(self) :: free                                   !< Free the units system.
    procedure, pass(self) :: list_constants                         !< Return the list of defined constants.
    procedure, pass(self) :: list_prefixes                          !< Return the list of defined prefixes.
    procedure, pass(self) :: list_units                             !< Return the list of defined units.
    procedure, pass(self) :: unit                                   !< Return an instance of the queried unit (if defined).
    generic               :: qunit => qunit_qreal                   !< Return an instance of quantity with the queried unit.
    ! private methods
    procedure, pass(self), private :: add_constant_qreal       !< Add a new constant from [[qreal]] input.
    procedure, pass(self), private :: add_prefix_string        !< Add a new prefix from string input.
    procedure, pass(self), private :: add_prefix_uom_reference !< Add a new prefix from [[uom_reference]] input.
    procedure, pass(self), private :: add_unit_string          !< Add a new unit from string input.
    procedure, pass(self), private :: add_unit_uom             !< Add a new unit from [[uom]] input.
    procedure, pass(self), private :: has_constant             !< Check if a constant is present into the system.
    procedure, pass(self), private :: has_prefix               !< Check if a prefix is present into the system.
    procedure, pass(self), private :: has_unit                 !< Check if a unit is present into the system.
    procedure, pass(self), private :: qunit_qreal              !< Return an instance of quantity with the queried unit.
endtype system_abstract

abstract interface
  !< Initialize the units system.
  subroutine initialize_interface(self, acronym)
  !< Initialize the units system.
  import system_abstract
  class(system_abstract), intent(inout)         :: self    !< The units system.
  character(*),           intent(in),  optional :: acronym !< Units system acronym, e.g. "SI" for the International System.
  endsubroutine initialize_interface
endinterface

contains
  ! public methods
  function const(self, c) result(const_)
  !< Return an instance of the queried constant (if defined).
  class(system_abstract), intent(in) :: self       !< The system.
  character(*),           intent(in) :: c          !< Constant name.
  type(qreal)                        :: const_     !< The queried constant.
  logical                            :: is_found   !< Flag to check if queried constant has been found.
  integer(I_P)                       :: co         !< Counter.

  is_found = .false.
  if (self%constants_number>0) then
    do co=1, self%constants_number
      if (self%constants(co)%has_name()) then
        if (trim(adjustl(c))==self%constants(co)%name) then
          const_ = self%constants(co)
          is_found = .true.
          exit
        endif
      endif
    enddo
  endif
  if (.not.is_found) then
    ! raise an error
  endif
  endfunction const

  elemental subroutine free(self)
  !< Free the units system.
  class(system_abstract), intent(inout) :: self !< The system.

  if (allocated(self%acronym)) deallocate(self%acronym)
  if (allocated(self%constants)) deallocate(self%constants)
  self%constants_number = 0_I_P
  if (allocated(self%units)) deallocate(self%units)
  self%units_number = 0_I_P
  if (allocated(self%prefixes)) deallocate(self%prefixes)
  self%prefixes_number = 0_I_P
  endsubroutine free

  function list_constants(self, with_dimensions, with_aliases, with_name, compact_reals, prefix_string) result(raw)
  !< Return the list defined constants.
  class(system_abstract), intent(in)           :: self            !< The system.
  logical,                intent(in), optional :: with_dimensions !< Flag to activate dimensions printing.
  logical,                intent(in), optional :: with_aliases    !< Flag to activate aliases printing.
  logical,                intent(in), optional :: with_name       !< Flag to activate name printing.
  logical,                intent(in), optional :: compact_reals   !< Flag to activate real numbers compacting.
  character(*),           intent(in), optional :: prefix_string   !< Prefix string.
  character(len=:), allocatable                :: raw             !< Raw characters data.
  character(len=:), allocatable                :: prefix_string_  !< Prefix string, local variable.
  integer(I_P)                                 :: c               !< Counter.

  raw = ''
  if (self%constants_number>0) then
    prefix_string_ = '' ; if (present(prefix_string)) prefix_string_ = prefix_string
    do c=1, self%constants_number
      raw = raw//new_line('a')//prefix_string_//'  '//trim(strz(n=c, nz_pad=3))//'. '//': '//&
            self%constants(c)%stringify(with_dimensions=with_dimensions,                     &
                                        with_aliases=with_aliases,                           &
                                        with_name=with_name,                                 &
                                        compact_reals=compact_reals)
    enddo
    raw = raw(2:)
  endif
  endfunction list_constants

  function list_prefixes(self, with_aliases, compact_reals, prefix_string) result(raw)
  !< Return the list defined prefixes.
  class(system_abstract), intent(in)           :: self           !< The system.
  logical,                intent(in), optional :: with_aliases   !< Flag to activate alias printing.
  logical,                intent(in), optional :: compact_reals  !< Flag to activate real numbers compacting.
  character(*),           intent(in), optional :: prefix_string  !< Prefix string.
  character(len=:), allocatable                :: raw            !< Raw characters data.
  character(len=:), allocatable                :: prefix_string_ !< Prefix string, local variable.
  integer(I_P)                                 :: p              !< Counter.

  raw = ''
  if (self%prefixes_number>0) then
    prefix_string_ = '' ; if (present(prefix_string)) prefix_string_ = prefix_string
    raw = prefix_string_//'Prefixes'
    do p=1, self%prefixes_number
      raw = raw//new_line('a')//prefix_string_//'  '//trim(strz(n=p, nz_pad=3))//'. '//&
            self%prefixes(p)%stringify(with_aliases=with_aliases, compact_reals=compact_reals)
    enddo
  endif
  endfunction list_prefixes

  function list_units(self, with_dimensions, with_aliases, protect_aliases, with_name, compact_reals, prefix_string) result(raw)
  !< Return the list defined units.
  class(system_abstract), intent(in)           :: self            !< The system.
  logical,                intent(in), optional :: with_dimensions !< Flag to activate dimensions printing.
  logical,                intent(in), optional :: with_aliases    !< Flag to activate aliases printing.
  logical,                intent(in), optional :: protect_aliases !< Flag to activate aliases printing in protected mode.
  logical,                intent(in), optional :: with_name       !< Flag to activate name printing.
  logical,                intent(in), optional :: compact_reals   !< Flag to activate real numbers compacting.
  character(*),           intent(in), optional :: prefix_string   !< Prefix string.
  character(len=:), allocatable                :: raw             !< Raw characters data.
  character(len=:), allocatable                :: prefix_string_  !< Prefix string, local variable.
  integer(I_P)                                 :: u               !< Counter.

  raw = ''
  if (self%units_number>0) then
    prefix_string_ = '' ; if (present(prefix_string)) prefix_string_ = prefix_string
    do u=1, self%units_number
      raw = raw//new_line('a')//prefix_string_//'  '//trim(strz(n=u, nz_pad=3))//'. '//': '//&
            self%units(u)%stringify(with_dimensions=with_dimensions,                         &
                                    with_aliases=with_aliases,                               &
                                    protect_aliases=protect_aliases,                         &
                                    with_name=with_name,                                     &
                                    compact_reals=compact_reals)
    enddo
    raw = raw(2:)
  endif
  endfunction list_units

  function unit(self, u) result(unit_)
  !< Return an instance of the queried unit (if defined).
  !<
  !< The query matches the unit name or a synonym of its main symbol, that is an alias with the same exponent, without
  !< multiplicative factor, additive offset or user-supplied converter (e.g. "meter" of "m = metre = meter"). The units
  !< obtained prefixing the system units by the system prefixes are queried too (e.g. "km", "kilometre"). The unit
  !< returned is undefined if the query matches no unit.
  class(system_abstract), intent(in) :: self   !< The system.
  character(*),           intent(in) :: u      !< Unit name or symbol.
  type(uom)                          :: unit_  !< The queried unit.
  type(uom)                          :: punit_ !< Prefixed unit.
  integer(I_P)                       :: un     !< Counter.
  integer(I_P)                       :: p      !< Counter.

  do un=1, self%units_number
    if (is_queried(unit=self%units(un), query=trim(adjustl(u)))) then
      unit_ = self%units(un)
      return
    endif
  enddo
  do p=1, self%prefixes_number
    do un=1, self%units_number
      call punit_%unset
      punit_ = self%units(un)%prefixed(prefixes=self%prefixes(p))
      if (is_queried(unit=punit_, query=trim(adjustl(u)))) then
        unit_ = punit_
        return
      endif
    enddo
  enddo
  endfunction unit

  ! private methods
  subroutine add_constant_qreal(self, source)
  !< Add a new constant from [[qreal]] input.
  class(system_abstract), intent(inout) :: self         !< The system.
  type(qreal),            intent(in)    :: source       !< Constant source.
  type(qreal), allocatable              :: constants(:) !< Defined constants.

  if (.not.self%has_constant(const=source)) then
    if (self%constants_number>0) then
      allocate(constants(1:self%constants_number+1_I_P))
      constants(1:self%constants_number) = self%constants
      constants(self%constants_number+1) = source
      call move_alloc(from=constants, to=self%constants)
      self%constants_number = self%constants_number + 1_I_P
    else
      self%constants_number = 1_I_P
      allocate(self%constants(1_I_P))
      self%constants(1) = source
    endif
  endif
  endsubroutine add_constant_qreal

  subroutine add_prefix_string(self, source)
  !< Add a new prefix from string input.
  class(system_abstract), intent(inout) :: self   !< The system.
  character(*),           intent(in)    :: source !< Prefix source.

  call self%add_prefix_uom_reference(source=uom_reference(source))
  endsubroutine add_prefix_string

  subroutine add_prefix_uom_reference(self, source)
  !< Add a new prefix from [[uom_reference]] input.
  class(system_abstract), intent(inout) :: self        !< The system.
  type(uom_reference),    intent(in)    :: source      !< Prefix source.
  type(uom_reference), allocatable      :: prefixes(:) !< Defined prefixes.
  integer(I_P)                          :: p           !< Counter.

  if (.not.self%has_prefix(prefix=source)) then
    if (self%prefixes_number>0) then
      allocate(prefixes(1:self%prefixes_number+1_I_P))
      do p=1, self%prefixes_number
        prefixes(p) = self%prefixes(p)
      enddo
      prefixes(self%prefixes_number+1) = source
      call move_alloc(from=prefixes, to=self%prefixes)
      self%prefixes_number = self%prefixes_number + 1_I_P
    else
      self%prefixes_number = 1_I_P
      allocate(self%prefixes(1_I_P))
      self%prefixes(1) = source
    endif
  endif
  endsubroutine add_prefix_uom_reference

  subroutine add_unit_string(self, source)
  !< Add a new unit from string input.
  class(system_abstract), intent(inout) :: self   !< The system.
  character(*),           intent(in)    :: source !< Unit source.

  call self%add_unit_uom(source=uom(source))
  endsubroutine add_unit_string

  subroutine add_unit_uom(self, source)
  !< Add a new unit from [[uom]] input.
  class(system_abstract), intent(inout) :: self     !< The system.
  type(uom),              intent(in)    :: source   !< Unit source.
  type(uom), allocatable                :: units(:) !< Defined units.

  if (.not.self%has_unit(unit=source)) then
    if (self%units_number>0) then
      allocate(units(1:self%units_number+1_I_P))
      units(1:self%units_number) = self%units
      units(self%units_number+1) = source
      call move_alloc(from=units, to=self%units)
      self%units_number = self%units_number + 1_I_P
    else
      self%units_number = 1_I_P
      allocate(self%units(1_I_P))
      self%units(1) = source
    endif
  endif
  endsubroutine add_unit_uom

  function has_constant(self, const) result(is_found)
  !< Return an instance of the queried unit (if defined).
  class(system_abstract), intent(in) :: self     !< The system.
  type(qreal),            intent(in) :: const    !< Constant queried.
  logical                            :: is_found !< Flag to check if queried constant has been found.
  integer(I_P)                       :: c        !< Counter.

  is_found = .false.
  if (self%constants_number>0) then
    do c=1, self%constants_number
      is_found = const == self%constants(c)
      if (is_found) exit
    enddo
  endif
  endfunction has_constant

  function has_prefix(self, prefix) result(is_found)
  !< Check if a prefix is present into the system.
  class(system_abstract), intent(in) :: self     !< The system.
  type(uom_reference),    intent(in) :: prefix   !< prefix queried.
  logical                            :: is_found !< Flag to check if queried prefix has been found.
  integer(I_P)                       :: u        !< Counter.

  is_found = .false.
  if (self%prefixes_number>0) then
    do u=1, self%prefixes_number
      is_found = prefix == self%prefixes(u)
      if (is_found) exit
    enddo
  endif
  endfunction has_prefix

  function has_unit(self, unit) result(is_found)
  !< Check if a unit is present into the system.
  class(system_abstract), intent(in) :: self     !< The system.
  type(uom),              intent(in) :: unit     !< Unit queried.
  logical                            :: is_found !< Flag to check if queried unit has been found.
  integer(I_P)                       :: u        !< Counter.

  is_found = .false.
  if (self%units_number>0) then
    do u=1, self%units_number
      is_found = unit == self%units(u)
      if (is_found) exit
    enddo
  endif
  endfunction has_unit

  function qunit_qreal(self, u) result(qunit_)
  !< Return an instance of quantity (qreal) with the queried unit (if defined).
  !<
  !< This is useful to build quantity by algebric expressions, e.g. `q1 = 2.0 * system%qunit('metre')`.
  class(system_abstract), intent(in) :: self   !< The system.
  character(*),           intent(in) :: u      !< Unit name or main symbol alias.
  type(qreal)                        :: qunit_ !< The queried quantity.
  type(uom)                          :: unit   !< Unit queried.

  unit = self%unit(u)
  if (unit%is_defined()) then
    qunit_ = qreal(magnitude=1._RKP, unit=unit)
  else
    ! raise an error
  endif
  endfunction qunit_qreal

  ! non type-bound procedures
  function is_queried(unit, query)
  !< Check if a unit is the queried one: by its name or by a synonym of its main symbol.
  !<
  !< The synonyms are searched into the main alias of the unit (e.g. "N" of "kg.m.s-2 (N[force])") and into its reference
  !< when it is the only one (e.g. "min" of "minute = 60 * s = min (minute[time])"): a unit made of more references (e.g.
  !< "m [length].s-1 [time-1]") is matched by its alias or name only.
  type(uom),    intent(in)              :: unit          !< The unit.
  character(*), intent(in)              :: query         !< Unit name or symbol.
  logical                               :: is_queried    !< Check result.
  type(uom_reference)                   :: alias         !< Main alias of the unit.
  type(uom_reference),      allocatable :: references(:) !< References of the unit.

  is_queried = .false.
  if (.not.unit%is_defined()) return
  if (unit%has_name()) then
    if (unit%name==query) then
      is_queried = .true.
      return
    endif
  endif
  if (unit%has_alias()) then
    call unit%get_alias(alias=alias)
    is_queried = has_synonym(reference=alias, query=query)
    if (is_queried) return
  endif
  call unit%get_references(references=references)
  if (allocated(references)) then
    if (size(references)==1) is_queried = has_synonym(reference=references(1), query=query)
  endif
  endfunction is_queried

  function has_synonym(reference, query)
  !< Check if a reference has a synonym of its main symbol equal to the query.
  !<
  !< A synonym is an alias with the same exponent of the main symbol, without multiplicative factor, additive offset or
  !< user-supplied converter: "meter" is a synonym of "m = metre = meter", "s" is not a synonym of "Hz = s-1".
  type(uom_reference), intent(in)   :: reference   !< The reference.
  character(*),        intent(in)   :: query       !< Symbol queried.
  logical                           :: has_synonym !< Check result.
  type(uom_symbol),     allocatable :: aliases(:)  !< Symbols of the reference.
  class(uom_converter), allocatable :: convert     !< User-supplied converter of a symbol.
  integer(I_P)                      :: a           !< Counter.

  has_synonym = .false.
  call reference%get_aliases(aliases=aliases)
  if (.not.allocated(aliases)) return
  do a=1, size(aliases)
    if (aliases(a)%get_symbol()/=query) cycle
    if (aliases(a)%get_exponent()/=aliases(1)%get_exponent()) cycle
    if (aliases(a)%get_factor()/=1._RKP.or.aliases(a)%get_offset()/=0._RKP) cycle
    call aliases(a)%get_convert(convert_=convert)
    if (allocated(convert)) cycle
    has_synonym = .true.
    return
  enddo
  endfunction has_synonym
