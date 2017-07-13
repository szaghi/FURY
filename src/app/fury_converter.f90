!< Command line converter built on top of FURY library.
program fury_converter
!< Command line converter built on top of FURY library.
!-----------------------------------------------------------------------------------------------------------------------------------
use, intrinsic :: iso_fortran_env, only : real64
use flap
use fury
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
real(real64)      :: converted_value   !< Value to be converted.
character(99)     :: input_uom_string  !< Input unit of measure as string.
character(99)     :: output_uom_string !< Output unit of measure as string.
type(uom64)       :: input_uom         !< Input unit of measure.
type(uom64)       :: output_uom        !< Output unit of measure.
type(qreal64)     :: input_quantity    !< Input quantity, value with uom.
type(qreal64)     :: output_quantity   !< Output quantity, value with uom.
type(system_si64) :: si                !< SI system.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
call si%initialize
call parse_cli
input_uom = si%unit(trim(adjustl(input_uom_string)))
output_uom = si%unit(trim(adjustl(output_uom_string)))
input_quantity = converted_value * input_uom
output_quantity = input_quantity%to(output_uom)
print "(A)", 'Input quantity: '//input_quantity%stringify(compact_reals=.true.)
print "(A)", 'Output (converted) quantity: '//output_quantity%stringify(compact_reals=.true.)
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  subroutine parse_cli()
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Setup and parse command line interface.
  !---------------------------------------------------------------------------------------------------------------------------------
  type(command_line_interface) :: cli   !< Command line interface handler.
  integer(I4P)                 :: error !< Error handler.
  !---------------------------------------------------------------------------------------------------------------------------------
  call cli%init(progname    = 'fury_converter',                     &
                authors     = 'Stefano Zaghoi',                     &
                license     = 'GNU GPLv3',                          &
                description = 'Babele units of measures converter', &
                examples    = ["fury_converter -iu 'km.h-1' -ou 'm.s-1' -v 3.9"])
  call cli%add(switch='--input_uom', switch_ab='-iu', help='input unit of measure', required=.true., act='store')
  call cli%add(switch='--output_uom', switch_ab='-ou', help='output unit of measure', required=.true., act='store')
  call cli%add(switch='--value', switch_ab='-val', help='value to be converted', required=.true., act='store')
  call cli%parse(error=error)
  call cli%get(switch='-iu', val=input_uom_string, error=error) ; if (error/=0) stop
  call cli%get(switch='-ou', val=output_uom_string, error=error) ; if (error/=0) stop
  call cli%get(switch='-v', val=converted_value, error=error) ; if (error/=0) stop
  endsubroutine parse_cli
endprogram fury_converter
