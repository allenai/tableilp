#!/bin/bash

# Include datastore credentials.
source /opt/ops/var/s3/ops-keystore/aws/datastore/credentials.sh

JVM_ARGS="-Xms4g -Xmx4g"

CLASS_NAME="org.allenai.ari.solvers.tableilp.TableIlpServer"

SCRIPT_DIR=`dirname $0`
SHORT_NAME=`basename $0 .sh`

# absolute paths for dynamic libraries for the SCIP ILP engine, for unix and osx
SCIP_LIBDIR="$PWD/$SCRIPT_DIR/../lib"
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$SCIP_LIBDIR"
export DYLD_LIBRARY_PATH="$DYLD_LIBRARY_PATH:$SCIP_LIBDIR"

. "${SCRIPT_DIR}/run-class.sh" "$CLASS_NAME" "$SHORT_NAME" "$@"
