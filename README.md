## An R interface for Mercury

The module is labelled 'ri' for 'R Inteface'.

### Prerequisites

+ A recent version development of R (> 2.4), installed with common
  dependencies.    
  
+  If R is not at least 4.0, the header files in the
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

            $ mmc … --search-lib-files-dir <rootdir> \   
                    --init-file <rootdir>/ri.init \    
                    --link-object <rootdir>/libri.a \    
                  …    

You need to make sure the library ‘libri.a’ and the main program were
compiled in the same grade.   
   
