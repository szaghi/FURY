!< FURY class definition of unit reference with float32 kind.

module fury_uom_reference32
!< FURY class definition of unit reference with float32 kind.
use, intrinsic :: iso_fortran_env, only : stderr => error_unit
use fury_uom_converter
use fury_uom_symbol32
use penf, RKP => R4P
use stringifor

#include "fury_uom_reference.inc"
endmodule fury_uom_reference32
