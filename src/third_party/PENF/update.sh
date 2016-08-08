#!/bin/bash

# Update PENF third party dependency

wget https://github.com/szaghi/PENF/blob/master/README.md -O README.md
mkdir -p src/lib
cd src/lib
wget https://github.com/szaghi/PENF/blob/master/src/lib/penf.F90 -O penf.F90
cd -
