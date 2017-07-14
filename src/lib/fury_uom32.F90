!< FURY definition of unit of measure class with float32 real kind.

module fury_uom32
!< FURY definition of unit of measure class with float32 real kind.
use, intrinsic :: iso_fortran_env, only : stderr => error_unit
use fury_uom_converter
use fury_uom_reference32
use fury_uom_symbol32
use penf, RKP => R4P
use stringifor

#include "fury_uom.inc"
endmodule fury_uom32
