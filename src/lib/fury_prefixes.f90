!< FURY definition prefixes class.
module fury_prefixes
!-----------------------------------------------------------------------------------------------------------------------------------
!< FURY definition prefixes class.
!-----------------------------------------------------------------------------------------------------------------------------------
use penf
use stringifor
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: prefixes
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: prefixes
  !< Prefixes class.
  type(string), allocatable :: aliases(:)           !< Litteral prefixes, e.g. "kilo, k" for e3 factor.
  integer(I_P)              :: aliases_number=0_I_P !< Aliases number.
  real(R_P)                 :: factor=1._R_P        !< Multiplicative factor, e.g. e3 for "kilo, k" prefix.
  contains
    ! public methods
    procedure, pass(self) :: free       !< Free the units system.
    procedure, pass(self) :: is_defined !< Check if the prefixes is defined.
    procedure, pass(self) :: stringify  !< Return a string representation of the prefixes.
endtype prefixes

interface prefixes
  !< Ovearloading [[prefixes]] name with a set of creator functions.
  module procedure creator
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! private non type bound procedure
  function creator(aliases, factor) result(prefixes_)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Create an instance of [[prefixes]]
  !---------------------------------------------------------------------------------------------------------------------------------
  character(*), intent(in) :: aliases(:) !< Litteral prefixes, e.g. "kilo, k" for e3 factor.
  real(R_P),    intent(in) :: factor     !< Multiplicative factor, e.g. e3 for "kilo, k" prefix.
  type(prefixes)           :: prefixes_  !< Prefixes.
  integer(I_P)             :: a          !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  prefixes_%aliases_number = size(aliases, dim=1)
  allocate(prefixes_%aliases(prefixes_%aliases_number))
  do a=1, prefixes_%aliases_number
    prefixes_%aliases(a) = trim(adjustl(aliases(a)))
  enddo
  prefixes_%factor = factor
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction creator

  ! public methods
  elemental subroutine free(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Free dynamic memory.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(prefixes), intent(inout) :: self !< Prefixes.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%aliases)) deallocate(self%aliases)
  self%aliases_number = 0_I_P
  self%factor = 1._R_P
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine free

  elemental function is_defined(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if the prefixes has been defined, namely it has defined prefixes.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(prefixes), intent(in) :: self       !< Prefixes.
  logical                     :: is_defined !< Definition status.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  is_defined = allocated(self%aliases)
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction is_defined

  function stringify(self, with_aliases, compact_reals) result(raw)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return a string representation of the prefixes.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(prefixes), intent(in)           :: self            !< Prefixes.
  logical,         intent(in), optional :: with_aliases    !< Flag to activate aliases printing.
  logical,         intent(in), optional :: compact_reals   !< Flag to activate real numbers compacting.
  character(len=:), allocatable         :: raw             !< Raw characters data.
  integer(I_P)                          :: a               !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_defined()) then
    raw = self%aliases(1)//' = '//trim(str(n=self%factor, compact=compact_reals))
    if (present(with_aliases)) then
      if (with_aliases.and.self%aliases_number>1) then
        do a=2, self%aliases_number
          raw = raw//' = '//self%aliases(a)
        enddo
      endif
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction stringify
endmodule fury_prefixes
