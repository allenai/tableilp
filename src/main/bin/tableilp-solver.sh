#!/bin/bash

JVM_ARGS="-Xms4g -Xmx4g"

CLASS_NAME="org.allenai.ari.solvers.tableilp.TableIlpServer"

SCRIPT_DIR=`dirname $0`
SHORT_NAME=`basename $0 .sh`

# dynamic libraries for the SCIP ILP engine, on unix and osx
SCIP_LIBDIR="$PWD/$SCRIPT_DIR/../lib"
LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$SCIP_LIBDIR"
DYLD_LIBRARY_PATH="$DYLD_LIBRARY_PATH:$SCIP_LIBDIR"
export LD_LIBRARY_PATH DYLD_LIBRARY_PATH

. "${SCRIPT_DIR}/run-class.sh" "$CLASS_NAME" "$SHORT_NAME" "$@"
