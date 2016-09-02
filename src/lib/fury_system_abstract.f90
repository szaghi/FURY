!< FURY definition of abstract units system.
module fury_system_abstract
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY definition of abstract units system.
!-----------------------------------------------------------------------------------------------------------------------------------
use fury_prefixes
use fury_uom
use penf
use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: system_abstract
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, abstract :: system_abstract
  !< Abstract units system class.
  character(len=:), allocatable :: acronym                       !< Units system acronym, e.g. "SI" for the International System.
  type(uom),        allocatable :: units(:)                      !< Defined units.
  integer(I_P)                  :: units_number=0_I_P            !< Number of units.
  type(prefixes), allocatable   :: decimal_prefixes(:)           !< Decimal prefixes.
  integer(I_P)                  :: decimal_prefixes_number=0_I_P !< Number of decimal prefixes.
  type(prefixes), allocatable   :: binary_prefixes(:)            !< Binary prefixes.
  integer(I_P)                  :: binary_prefixes_number=0_I_P  !< Number of binary prefixes.
  contains
    ! public deferred methods
    procedure(initialize_interface), pass(self), deferred :: initialize !< Initialize the units system.
    ! public methods
    generic               :: add_unit => add_unit_string, add_unit_uom !< Add a new unit.
    procedure, pass(self) :: free                                      !< Free the units system.
    procedure, pass(self) :: list_prefixes                             !< Return the list of defined prefixes.
    procedure, pass(self) :: list_units                                !< Return the list of defined units.
    procedure, pass(self) :: unit                                      !< Return an instance of the queried unit (if defined).
    ! private methods
    procedure, pass(self), private :: add_unit_string !< Add a new unit from string input.
    procedure, pass(self), private :: add_unit_uom    !< Add a new unit from [[uom]] input.
endtype system_abstract

abstract interface
  !< Initialize the units system.
  subroutine initialize_interface(self, acronym)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Initialize the units system.
  !---------------------------------------------------------------------------------------------------------------------------------
  import system_abstract
  class(system_abstract), intent(inout)         :: self    !< The units system.
  character(*),           intent(in),  optional :: acronym !< Units system acronym, e.g. "SI" for the International System.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine initialize_interface
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! public methods
  elemental subroutine free(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Free the units system.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(system_abstract), intent(inout) :: self !< The system.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%acronym)) deallocate(self%acronym)
  if (allocated(self%units)) deallocate(self%units)
  self%units_number = 0_I_P
  if (allocated(self%decimal_prefixes)) deallocate(self%decimal_prefixes)
  self%decimal_prefixes_number = 0_I_P
  if (allocated(self%binary_prefixes)) deallocate(self%binary_prefixes)
  self%binary_prefixes_number = 0_I_P
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine free

  function list_prefixes(self, with_aliases, compact_reals, prefix_string) result(raw)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the list defined prefixes.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(system_abstract), intent(in)           :: self           !< The system.
  logical,                intent(in), optional :: with_aliases   !< Flag to activate alias printing.
  logical,                intent(in), optional :: compact_reals  !< Flag to activate real numbers compacting.
  character(*),           intent(in), optional :: prefix_string  !< Prefix string.
  character(len=:), allocatable                :: raw            !< Raw characters data.
  character(len=:), allocatable                :: prefix_string_ !< Prefix string, local variable.
  integer(I_P)                                 :: p              !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  raw = ''
  if (self%decimal_prefixes_number>0) then
    prefix_string_ = '' ; if (present(prefix_string)) prefix_string_ = prefix_string
    raw = prefix_string_//'Decimal Prefixes'
    do p=1, self%decimal_prefixes_number
      raw = raw//new_line('a')//prefix_string_//'  '//trim(strz(n=p, nz_pad=3))//'. '//&
            self%decimal_prefixes(p)%stringify(with_aliases=with_aliases, compact_reals=compact_reals)
    enddo
  endif
  if (self%binary_prefixes_number>0) then
    prefix_string_ = '' ; if (present(prefix_string)) prefix_string_ = prefix_string
    if (raw/='') then
      raw = raw//new_line('a')//prefix_string_//'Binary Prefixes'
    else
      raw = prefix_string_//'Binary Prefixes'
    endif
    do p=1, self%binary_prefixes_number
      raw = raw//new_line('a')//prefix_string_//'  '//trim(strz(n=p, nz_pad=3))//'. '//&
            self%binary_prefixes(p)%stringify(with_aliases=with_aliases, compact_reals=compact_reals)
    enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction list_prefixes

  function list_units(self, with_dimensions, with_aliases, protect_aliases, with_name, compact_reals, prefix_string) result(raw)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the list defined units.
  !---------------------------------------------------------------------------------------------------------------------------------
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
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
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
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction list_units

  function unit(self, u) result(unit_)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return an instance of the queried unit (if defined).
  !---------------------------------------------------------------------------------------------------------------------------------
  class(system_abstract), intent(in) :: self       !< The system.
  character(*),           intent(in) :: u          !< Unit name or main symbol alias.
  type(uom)                          :: unit_      !< The queried unit.
  type(uom)                          :: punit_     !< Prefixed unit.
  type(string)                       :: symbol     !< Unit symbol.
  logical                            :: is_found   !< Flag to check if queried unit has been found.
  integer(I_P)                       :: un         !< Counter.
  integer(I_P)                       :: p          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_found = .false.
  if (self%units_number>0) then
    do un=1, self%units_number
      if (self%units(un)%is_defined()) then
        if (self%units(un)%name==trim(adjustl(u))) then
          unit_ = self%units(un)
          is_found = .true.
          exit
        else
          symbol = self%units(un)%get_main_symbol()
          if (symbol==trim(adjustl(u))) then
            unit_ = self%units(un)
            is_found = .true.
            exit
          endif
        endif
      endif
    enddo
    if (.not.is_found.and.self%decimal_prefixes_number>0) then
      prefix_loop: do p=1, self%decimal_prefixes_number
        do un=1, self%units_number
          call punit_%unset()
          punit_ = self%units(un)%prefixed_unit(prefix=self%decimal_prefixes(p))
          if (punit_%name==trim(adjustl(u))) then
            unit_ = punit_
            is_found = .true.
            exit prefix_loop
          else
            symbol = punit_%get_main_symbol()
            if (symbol==trim(adjustl(u))) then
              unit_ = punit_
              is_found = .true.
              exit prefix_loop
            endif
          endif
        enddo
      enddo prefix_loop
    endif
  endif
  if (.not.is_found) then
    ! raise an error
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction unit

  ! private methods
  subroutine add_unit_string(self, source)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add a new unit from string input.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(system_abstract), intent(inout) :: self   !< The system.
  character(*),           intent(in)    :: source !< Unit source.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%add_unit_uom(source=uom(source))
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_unit_string

  subroutine add_unit_uom(self, source)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add a new unit from [[uom]] input.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(system_abstract), intent(inout) :: self     !< The system.
  type(uom),              intent(in)    :: source   !< Unit source.
  type(uom), allocatable                :: units(:) !< Defined units.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
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
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_unit_uom
endmodule fury_system_abstract
