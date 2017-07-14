!< FURY class definition of unit reference with float128 kind.

module fury_uom_reference128
!< FURY class definition of unit reference with float128 kind.
use, intrinsic :: iso_fortran_env, only : stderr => error_unit
use fury_uom_converter
use fury_uom_symbol128
use penf, RKP => R16P
use stringifor

#include "fury_uom_reference.inc"
endmodule fury_uom_reference128
