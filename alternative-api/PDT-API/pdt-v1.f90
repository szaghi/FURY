 module mykinds_m

   use, intrinsic :: iso_fortran_env, only : WP => real64

   implicit none

   private

   public :: WP

end module mykinds_m

module uomtype_m

   use mykinds_m, only : WP

   implicit none

   private

   integer, parameter, public :: LENUOMLABEL = 12

   type, public :: uomtype_t(MASS,LENGTH,TIME)
      integer, kind :: MASS = 0   ! Default type is dimensionless
      integer, kind :: LENGTH = 0 !
      integer, kind :: TIME = 0   !
      private
      real(WP), public :: Val = 0.0_wp
      character(len=LENUOMLABEL), public :: UomLabel = ""
   contains
      private
      procedure, pass(this) :: convert_uomtype_t
      generic, public :: convert => convert_uomtype_t
   end type uomtype_t

contains

   function convert_uomtype_t( this, label ) result( new_val )

      class(uomtype_t(MASS=0,LENGTH=0,TIME=0)), intent(in) :: this
      character(len=*), intent(in)   :: label
      !.. Function result
      real(WP) :: new_val

      !.. Local variables
      character(len=LENUOMLABEL) :: InLabel

      !.. Some code to validate input
      InLabel = label
      new_val = this%Val

      !.. Appropriate conversion logic elided
      if ( trim(InLabel) /= this%UomLabel ) then
         ! error stop?
      end if

      return

   end function convert_uomtype_t

end module uomtype_m
module mass_m

   use mykinds_m, only : WP
   use uomtype_m, only : uomtype_t, LENUOMLABEL

   implicit none

   private

   type, extends(uomtype_t) :: mass_t
      private
   contains
      private
      procedure, pass(this) :: convert_mass_t
      generic, public :: convert => convert_mass_t
   end type mass_t

   interface mass_t
      module procedure construct_mass_t
   end interface
   public :: mass_t, construct_mass_t

   character(len=LENUOMLABEL), parameter :: MASS_LABELS(2) = [ character(len=LENUOMLABEL) :: "kg", "gm" ]

contains

   function construct_mass_t( val, label ) result( new_m )

      real(WP), intent(in)         :: val
      character(len=*), intent(in) :: label
      !.. Function result
      type(mass_t(MASS=1,LENGTH=0,TIME=0)) :: new_m

      !.. Some code to validate input
      new_m%UomLabel = label
      select case ( new_m%UomLabel )
         case ( MASS_LABELS(1):MASS_LABELS(size(MASS_LABELS)) )
         case default
            ! error stop?
      end select

      new_m%val = val

   end function construct_mass_t

   function convert_mass_t( this, label ) result( new_val )

      class(mass_t(MASS=1,LENGTH=0,TIME=0)), intent(in) :: this
      character(len=*), intent(in)                      :: label
      !.. Function result
      real(WP) :: new_val

      !.. Local variables
      character(len=LENUOMLABEL) :: InLabel

      !.. Some code to validate input
      InLabel = label
      new_val = this%Val

      !.. Appropriate conversion logic elided
      !   Here, assume value in passed object is in SI units
      select case ( InLabel )
         case ( MASS_LABELS(1) )
         case ( MASS_LABELS(2) )
            new_val = new_val * 1E3_wp
         case default
            ! error stop?
      end select

   end function convert_mass_t

end module mass_m
module length_m

   use mykinds_m, only : WP
   use uomtype_m, only : uomtype_t, LENUOMLABEL

   implicit none

   private

   type, extends(uomtype_t) :: length_t
      private
   contains
      private
      procedure, pass(this) :: convert_length_t
      generic, public :: convert => convert_length_t
   end type length_t

   interface length_t
      module procedure construct_length_t
   end interface
   public :: length_t, construct_length_t

   character(len=LENUOMLABEL), parameter :: LENGTH_LABELS(2) = [ character(len=LENUOMLABEL) :: "m", "cm" ]

contains

   function construct_length_t( val, label ) result( new_l )

      real(WP), intent(in)         :: val
      character(len=*), intent(in) :: label
      !.. Function result
      type(length_t(MASS=0,LENGTH=1,TIME=0)) :: new_l

      !.. Some code to validate input
      new_l%UomLabel = label
      select case ( new_l%UomLabel )
         case ( LENGTH_LABELS(1):LENGTH_LABELS(size(LENGTH_LABELS)) )
         case default
            ! error stop?
      end select

      new_l%val = val

   end function construct_length_t

   function convert_length_t( this, label ) result( new_val )

      class(length_t(MASS=0,LENGTH=1,TIME=0)), intent(in) :: this
      character(len=*), intent(in)     :: label
      !.. Function result
      real(WP) :: new_val

      !.. Local variables
      character(len=LENUOMLABEL) :: InLabel

      !.. Some code to validate input
      InLabel = label
      new_val = this%Val

      !.. Appropriate conversion logic elided
      !   Here, assume value in passed object is in SI units
      select case ( InLabel )
         case ( LENGTH_LABELS(1) )
         case ( LENGTH_LABELS(2) )
            new_val = new_val * 1E2_wp
         case default
            ! error stop?
      end select

   end function convert_length_t

end module length_m
module time_m

   use mykinds_m, only : WP
   use uomtype_m, only : uomtype_t, LENUOMLABEL

   implicit none

   private

   type, extends(uomtype_t) :: time_t
      private
   contains
      private
      procedure, pass(this) :: convert_time_t
      generic, public :: convert => convert_time_t
   end type time_t

   interface time_t
      module procedure construct_time_t
   end interface
   public :: time_t, construct_time_t

   character(len=LENUOMLABEL), parameter :: TIME_LABELS(3) = [ character(len=LENUOMLABEL) :: "s", "min", "hour" ]

contains

   function construct_time_t( val, label ) result( new_t )

      real(WP), intent(in)         :: val
      character(len=*), intent(in) :: label
      !.. Function result
      type(time_t(MASS=0,LENGTH=0,TIME=1)) :: new_t

      !.. Some code to validate input
      new_t%UomLabel = label
      select case ( new_t%UomLabel )
         case ( TIME_LABELS(1):TIME_LABELS(size(TIME_LABELS)) )
         case default
            ! error stop?
      end select

      new_t%val = val

   end function construct_time_t

   function convert_time_t( this, label ) result( new_val )

      class(time_t(MASS=0,LENGTH=0,TIME=1)), intent(in) :: this
      character(len=*), intent(in)   :: label
      !.. Function result
      real(WP) :: new_val

      !.. Local variables
      character(len=LENUOMLABEL) :: InLabel

      !.. Some code to validate input
      InLabel = label
      new_val = this%Val

      !.. Appropriate conversion logic elided
      !   Here, assume value in passed object is in SI units
      select case ( InLabel )
         case ( TIME_LABELS(1) )
         case ( TIME_LABELS(2) )
            new_val = new_val / 60.0_wp
         case ( TIME_LABELS(3) )
            new_val = new_val / 3600.0_wp
         case default
            ! error stop?
      end select

   end function convert_time_t

end module time_m
module velocity_m

   use mykinds_m, only : WP
   use uomtype_m, only : uomtype_t, LENUOMLABEL

   implicit none

   private

   type, extends(uomtype_t) :: velocity_t
      private
   contains
      private
      procedure, pass(this) :: convert_velocity_t
      generic, public :: convert => convert_velocity_t
   end type velocity_t

   interface velocity_t
      module procedure construct_velocity_t
   end interface
   public :: velocity_t , construct_velocity_t

contains

   function construct_velocity_t( val, label ) result( new_v )

      real(WP), intent(in)         :: val
      character(len=*), intent(in) :: label
      !.. Function result
      type(velocity_t(MASS=0,LENGTH=1,TIME=-1)) :: new_v

      !.. Procedure to validate label elided based on fundamental types of length and time
      !   If not validated, error stop?

      new_v%UomLabel = label
      new_v%val = val

   end function construct_velocity_t

   function convert_velocity_t( this, label ) result( new_val )

      class(velocity_t(MASS=0,LENGTH=1,TIME=-1)), intent(in) :: this
      character(len=*), intent(in)       :: label
      !.. Function result
      real(WP) :: new_val

      !.. Local variables
      character(len=LENUOMLABEL) :: InLabel

      !.. Some code to validate input
      InLabel = label
      new_val = this%Val

      !.. Appropriate conversion logic elided

      return

   end function convert_velocity_t

end module velocity_m
module acceleration_m

   use mykinds_m, only : WP
   use uomtype_m, only : uomtype_t, LENUOMLABEL

   implicit none

   private

   type, extends(uomtype_t) :: acceleration_t
      private
    contains
      private
      procedure, pass(this) :: convert_acceleration_t
      generic, public :: convert => convert_acceleration_t
   end type acceleration_t

   interface acceleration_t
      module procedure construct_acceleration_t
   end interface
   public :: acceleration_t, construct_acceleration_t

contains

   function construct_acceleration_t( val, label ) result( new_a )

      real(WP), intent(in)         :: val
      character(len=*), intent(in) :: label
      !.. Function result
      type(acceleration_t(MASS=0,LENGTH=1,TIME=-2)) :: new_a

      !.. Procedure to validate label elided based on fundamental types of length and time
      !   If not validated, error stop?

      new_a%UomLabel = label
      new_a%val = val

   end function construct_acceleration_t

   function convert_acceleration_t( this, label ) result( new_val )

      class(acceleration_t(MASS=0,LENGTH=1,TIME=-2)), intent(in) :: this
      character(len=*), intent(in)           :: label
      !.. Function result
      real(WP) :: new_val

      !.. Local variables
      character(len=LENUOMLABEL) :: InLabel

      !.. Some code to validate input
      InLabel = label
      new_val = this%Val

      !.. Appropriate conversion logic elided

      return

   end function convert_acceleration_t

end module acceleration_m
module force_m

   use mykinds_m, only : WP
   use uomtype_m, only : uomtype_t, LENUOMLABEL

   implicit none

   private

   type, extends(uomtype_t) :: force_t
      private
    contains
      private
      procedure, pass(this) :: convert_force_t
      generic, public :: convert => convert_force_t
   end type force_t

   interface force_t
      module procedure construct_force_t
   end interface
   public :: force_t, construct_force_t

contains

   function construct_force_t( val, label ) result( new_f )

      real(WP), intent(in)         :: val
      character(len=*), intent(in) :: label
      !.. Function result
      type(force_t(MASS=1,LENGTH=1,TIME=-2)) :: new_f

      !.. Procedure to validate label elided based on fundamental types of mass, length and time
      !   If not validated, error stop?

      new_f%UomLabel = label
      new_f%val = val

   end function construct_force_t

   function convert_force_t( this, label ) result( new_val )

      class(force_t(MASS=1,LENGTH=1,TIME=-2)), intent(in) :: this
      character(len=*), intent(in)    :: label
      !.. Function result
      real(WP) :: new_val

      !.. Local variables
      character(len=LENUOMLABEL) :: InLabel

      !.. Some code to validate input
      InLabel = label
      new_val = this%Val

      !.. Appropriate conversion logic elided

      return

   end function convert_force_t

end module force_m
module myunits_m

   use uomtype_m, only : uomtype_t
   use mass_m, only : mass_t, construct_mass_t
   use length_m, only : length_t            , construct_length_t
   use time_m, only : time_t                , construct_time_t
   use velocity_m, only : velocity_t        , construct_velocity_t
   use acceleration_m, only : acceleration_t, construct_acceleration_t
   use force_m, only : force_t              , construct_force_t

   implicit none

   private

   interface operator(*)
      module procedure multiply_ma
   end interface

   public :: mass_t         ,  construct_mass_t
   public :: length_t       ,  construct_length_t
   public :: time_t         ,  construct_time_t
   public :: velocity_t     ,  construct_velocity_t
   public :: acceleration_t ,  construct_acceleration_t
   public :: force_t        ,  construct_force_t
   public :: operator(*)

contains

   function multiply_ma( m, a ) result( F )

      class(uomtype_t(MASS=1,LENGTH=0,TIME=0)), intent(in)  :: m
      class(uomtype_t(MASS=0,LENGTH=1,TIME=-2)), intent(in) :: a
      !.. Function result
      type(force_t(MASS=1,LENGTH=1,TIME=-2)) :: F

      !.. Appropriate multiplication operation elided
      F%Val = m%val * a%Val
      F%UomLabel = trim(m%UomLabel) // "*" // trim(a%UomLabel)

      return

   end function multiply_ma

end module myunits_m

!.. Sample usage
program p

   use mykinds_m, only : WP
   use uomtype_m, only : uomtype_t
   use myunits_m, only : mass_t, construct_mass_t, &
                         velocity_t, construct_velocity_t, &
                         acceleration_t, construct_acceleration_t, &
                         force_t, construct_force_t, operator(*)

   implicit none

   type(mass_t(MASS=1,LENGTH=0,TIME=0))          :: m
   type(velocity_t(MASS=0,LENGTH=1,TIME=-1))     :: v
   type(acceleration_t(MASS=0,LENGTH=1,TIME=-2)) :: a
   type(force_t(MASS=1,LENGTH=1,TIME=-2))        :: F

   m = construct_mass_t( val=1.0_wp, label="kg" )

   v = construct_velocity_t( val=1.0_wp, label="m/s" )

   a = construct_acceleration_t( val=2.0_wp, label="m/s2" )

   F = m * a

   print *, "Force = ", F%val, F%UomLabel

   stop

end program p
