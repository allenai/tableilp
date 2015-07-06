# ILP Based Table Inference Solver

## Overview

This solver implements an Integer Linear Programming (ILP) based method to answer Aristo questions. The input knowledge it uses is in the form of Tables. For solving the ILP model, it uses the SCIP solver (http://scip.zib.de).


## Installing SCIP on Linux and Mac

While core SCIP solver builds smoothly from source with a simple `make` command, building its Java interface requires some edits for Linux and some additional changes for Mac.

* Download and unzip SCIP Optimization Suite source from http://scip.zib.de/#download; this guide is based on version 3.1.1.

* Disable GMP throughout the build process by using `make GMP=false`.

* Run `make GMP=false` to build the core SCIP solver, followed by `make test` to test. You may wish to use `-j8` to speed up the build.

* To build the Java/JNI interface, switch to the folder `scip-3.1.1/interfaces/jni`, and:

  1. In `Makefile`, change `-I$(LIBDIR)/jniinc` in `FLAGS` to point to the actual location of `jni.h` (which may be found with `locate jni.h` and which is where `jni_md.h` should also be located).

  2. In `Makefile`, change the `$(JNILIBFILE)` target to move `$(JNILIBOBJFILES)` higher up, just before the lines starting with `$(LINKCC_L)`.

  3. (FOR MAC ONLY) In `Makefile`, change the two `install` lines to make them Mac-compatible by dropping `-t $(LIBDIR)` from them and instead appending `$(LIBDIR)` as the last argument of each `install` command.

  4. Follow steps 1-6 in the `README` in this folder, remembering to use `GMP=false` with `make`.

  5. (FOR MAC ONLY) In the `lib` folder, make a few symbolic links:

    ```
    ln -s libjscip-0.1.darwin.x86_64.gnu.opt.spx.so libjscip-0.1.darwin.x86_64.gnu.opt.spx.dylib
    ln -s libsoplex.darwin.x86_64.gnu.opt.so libsoplex-2.0.1.darwin.x86_64.gnu.opt.so
    ln -s libscip.darwin.x86_64.gnu.opt.so libscip-3.1.1.darwin.x86_64.gnu.opt.so
    ln -s liblpispx.darwin.x86_64.gnu.opt.so liblpispx-3.1.1.darwin.x86_64.gnu.opt.so
    ln -s libnlpi.cppad.darwin.x86_64.gnu.opt.so libnlpi.cppad-3.1.1.darwin.x86_64.gnu.opt.so
    ```

  6. Switch to folder `examples/JniKnapsack`, and:

    a. (FOR MAC ONLY) In `run.sh`, change `LD_LIBRARY_PATH` to `DYLD_LIBRARY_PATH`.

    b. Compile with `make`.

    c. Test with `./run.sh`.


