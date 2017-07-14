!< FURY definition of unit of measure class with float128 real kind.

module fury_uom128
!< FURY definition of unit of measure class with float128 real kind.
use, intrinsic :: iso_fortran_env, only : stderr => error_unit
use fury_uom_converter
use fury_uom_reference128
use fury_uom_symbol128
use penf, RKP => R16P
use stringifor

#include "fury_uom.inc"
endmodule fury_uom128
