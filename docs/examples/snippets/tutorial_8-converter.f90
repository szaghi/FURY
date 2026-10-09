type, extends(uom_converter) :: dbm_to_mw
  contains
    procedure, nopass    :: convert_float128
    procedure, nopass    :: convert_float64
    procedure, nopass    :: convert_float32
    procedure, pass(lhs) :: assign_converter
endtype dbm_to_mw
