#!/bin/bash

# Update StringiFor third party dependency

wget https://github.com/szaghi/StringiFor/blob/master/README.md -O README.md
mkdir -p src/lib
cd src/lib
wget https://github.com/szaghi/StringiFor/tree/master/src/lib/befor64.F90              -O befor64.F90
wget https://github.com/szaghi/StringiFor/tree/master/src/lib/befor64_pack_data_m.F90  -O befor64_pack_data_m.F90
wget https://github.com/szaghi/StringiFor/tree/master/src/lib/stringifor.F90           -O stringifor.F90
wget https://github.com/szaghi/StringiFor/tree/master/src/lib/stringifor_string_t.F90  -O stringifor_string_t.F90
cd -
