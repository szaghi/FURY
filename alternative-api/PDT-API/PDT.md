### Parametrized Derived Type API

The concept of *dimensions* for physical quantities is very close to the concept of *kindness* of Fortran variables. As a consequence, an effective, concise API could be based on the exploitation of **Parametrized Derived Type** of Fortran (2003+). This directory contains the *prototype API* proposed by *FortranFan* (great) coder on a Google Group discussion. Unfortunately, some *mainstream* compilers like GNU gfortran (6.x at the date) do not yet support parametrized derived type, thus such an approach is currently non considered *viable* for FURY. Nevertheless, PDT should be considered a valid base for future version of FURY.

Two slightly different version of the prototype API are provided, namely `pdt-v1.f90` and `pdt-v2.f90~.
