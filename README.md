## An R interface for Mercury

The module is labelled 'ri' for 'R Interface'.

### Status

+ Preliminary design and programming completed.  
+ Currently the library builds and linked programs build with it 
(30 April 2021).  
+ univ-based procedures to be fixed.

### Prerequisites

+ A recent version development of R (> 2.4), installed with common
  dependencies.

+ R should have been built from source with:

        $ ./configure --enable-R-shlib

  The configuration file `config.h` should replace the one
  distributed at the root of the source directory.
  Alternatively, you may use a standard R distribution, provided
  that `libR.so` is installed. In this case, please adjust config.h
  manually to your platform paths and dependencies.

+ If R is not at least 4.0, the header files in the
  root directory should be replaced from a fresh R source package
  (they are not normally distributed as public headers).

+ GCC (8+)

### Build

It should be built as follows:

1. First check file Mercury options, and adjust paths and library
   names according to your platform specifications.

2. Run at the root of the source directory:

            $ mmc --make libri.install

3. If the build is successful, libraries are to be found under
    local/lib/mercury/lib/{*.gc}

    To build a Mercury project against these libraries:

            $ mmc … --search-lib-files-dir local/lib/mercury/lib/hlc.gc \
                    --init-file local/lib/mercury/modules/hlc.gc/ri.init \
                    --link-object local/lib/mercury/lib/hlc.gc/libri.a \
                    --ld-flags "-lR -lRblas" \
		    yourfile.m
                                   …
Change 'hlc.gc' to the appropriate build grade (like possibly 'asm_fast.gc').
You will need a GC grade (this is unchecked in the build process).
You will have to erase '-lRblas' from the above if R has been built without
libRblas.
You need to make sure the library ‘libri.a’ and the main program were compiled
in  the same grade.
