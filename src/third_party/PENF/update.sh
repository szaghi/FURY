#!/bin/bash

# Update PENF third party dependency

wget https://raw.githubusercontent.com/szaghi/PENF/master/README.md -O README.md
mkdir -p src/lib
cd src/lib
wget https://raw.githubusercontent.com/szaghi/PENF/master/src/lib/penf.F90 -O penf.F90
cd -
