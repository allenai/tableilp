# ILP Based Table Inference Solver

#### This code is just for *demonstration* of the ideas presented in [this](http://arxiv.org/abs/1604.06076) paper. Since some of the dependencies of this code are not publicly available, it is not possile to run it as it is. We hope to release a stand-alone version of this system in near future. 

## Overview

This solver implements an Integer Linear Programming (ILP; https://en.wikipedia.org/wiki/Integer_programming) based method to answer Aristo questions. The input knowledge it uses is in the form of Tables. For solving the ILP model, it uses the SCIP solver (http://scip.zib.de).

The ILP model consists of a set of integer (mostly binary) variables, a set of constraints on them, and an objective function.

* The *variables* encode similarity or entailment links between pairs of words (or chunks) in the question, answer choices, and KB tables (both cells and table titles). These links are weight noed by the lexical similarity between the words they connect, and a link turns "on" if it is part of a reasoning subgraph that connects the question to the KB tables and an answer choice.

* The *constraints* encode the conditions needed to create a "proper" reasoning chain between the question the the KB table, such as the reasoning chain being a connected subgraph, not having "dangling" connections that aren't on the path to the answer choice, at most *k* rows active per table, a cell having a higher priority to match if the corresponding table title matches, etc.

* The *objective function* is a maximization function that seeks the most connected reasoning chain supported by words in the question. It also includes priorities for other desirables such as more question coverage. 


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


