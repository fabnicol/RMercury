## An R interface library for Mercury

This project is a proof-of-concept for a
[Mercury](https://www.mercurylang.org) library allowing to call [R](https://www.r-project.org/),
in a way compatible with the declarative semantics of
the Mercury language.  
It will be implemented over time if some minor technical issues are
resolved.  

### Purpose

The objective is to let R C/C++/FORTRAN libraries do the heavy lifting
of numerical computations, data processing, input parsing and
graphic creation. The results of R processing are then imported to
perform logic programming over data tables converted to
predicative records.  

### Rationale

Inspired by the [SWI-Prolog `real`
package](https://github.com/SWI-Prolog/packages-real), by Nicos
Angelopoulous and Vitor Santos Costa.  
See brilliant developments therein.  
There are however differences linked to the fact that Mercury is a
compiled language: R code will be incorporated as strings into Mercury
code, not inlined as in the `real` SWI-Prolog package.

### Target

All users who need both logic or functional analysis of qualitatively
rich and complex data and good performance in a big data context, or
seamless access to statistical procedures and graphics offered by R.   

### TODO list (ranked):

    * Add processing of vectorized data types.
    * Add some support for exception handling.
    * Add R graphic output.
    * Add realistic test cases.
    * Reduce impurities caused by foreign C code.
    * Implement as a Mercury library, not a testbed.
    
## Installation guidelines

This project has been developed under GNU/Linux Ubuntu 20.10. Other
platforms are not supported.  
Build Mercury from source code using the 20.06.1 distribution and the
GCC compiler. I used `GNU gcc/g++ 10.2`. Other compilers may work
too but will not be supported.  

Install `R-4.0.2` or later version using `apt` or build from source. If
so, configure the source package with `--enable-R-shlib --prefix=/usr`.  

Install the following R packages: `RInside`, `data.table`.  
*Note: preferably use an admin install to /usr/lib, as local installs
seem to raise issues. You may first automatically install to $HOME/R
using the R packager then copy to /usr/lib/R/library by hand.*  

Donload the present repository, open a console in the local directory
and run GNU `make`.  

## Running the test executable

If all goes well, an executable named `test0` will be created by the
Mercury compiler (`mmc`) at the root of the directory. Run it as
follows:   

      LD_LIBRARY_PATH=/usr/lib/R/library/RInside/lib/  ./test0  

-----------------------------------

Fabrice Nicol, Jan. 2021


