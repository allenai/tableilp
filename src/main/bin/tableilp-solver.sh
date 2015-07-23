#!/bin/bash

JVM_ARGS="-Xms4g -Xmx4g"

CLASS_NAME="org.allenai.ari.solvers.tableilp.TableIlpServer"

SCRIPT_DIR=`dirname $0`
SHORT_NAME=`basename $0 .sh`
. "${SCRIPT_DIR}/run-class.sh" "$CLASS_NAME" "$SHORT_NAME" "$@"
