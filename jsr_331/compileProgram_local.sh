#!/bin/sh

# 2010-10-04/hakank@bonetmail.com

PROBLEM=$1
THIS_SOLVER=$2

THIS_PATH=/home/hakank/constraints/jsr_331/jsr331

echo "Problem:$PROBLEM Solver: $THIS_SOLVER"

if [ ! -e "$PROBLEM" ]; then
   echo "File $PROBLEM do not exists"
   exit;
fi

CONSTRAINER_SOLVER=$THIS_PATH/lib/constrainer/jsr331.constrainer.jar:$THIS_PATH/lib/constrainer/constrainer.light.jar
CHOCO_SOLVER=$THIS_PATH//lib/choco/jsr331.choco.jar:$THIS_PATH/lib/choco/choco-solver-2.1.1-20110622-with-sources.jar
JSETL_SOLVER=$THIS_PATH/lib/jsetl/jsr331.jsetl.jar:$THIS_PATH/lib/jsetl/jsetl.jar

# Default solver
SOLVER=$CONSTRAINER_SOLVER


if [ "$THIS_SOLVER" = "choco" ]; then
   echo "solver is choco"
   SOLVER=$CHOCO_SOLVER;
fi
if [ "$THIS_SOLVER" = "constrainer" ]; then
   echo "solver is constrainer"
   SOLVER=$CONSTRAINER_SOLVER;
fi
if [ "$THIS_SOLVER" = "jsetl" ]; then
   echo "solver is jsetl"
   SOLVER=$JSETL_SOLVER;
fi

PROGRAM=$PROBLEM
echo compile $PROGRAM $THIS_SOLVER
# export LOGLIBS=$THIS_PATH/lib/logging/commons-logging-1.1.jar:$THIS_PATH/lib/logging/commons-logging-api-1.1.jar:$THIS_PATH/lib/logging/log4j-1.2.15.jar
export LOGLIBS=$THIS_PATH/lib/logging/commons-logging-1.1.jar:$THIS_PATH/lib/logging/commons-logging-api-1.1.jar:$THIS_PATH/lib/logging/log4j-1.2.15.jar

export LIBS=$THIS_PATH/bin:$THIS_PATH/lib/jsr331.jar:$SOLVER:$LOGLIBS
# export LIBS=$THIS_PATH/bin:$THIS_PATH/lib/jsr331.jar:$SOLVER:$LOGLIBS
export MYCLASSPATH=".:$LIBS"
echo javac -classpath $MYCLASSPATH  $PROGRAM
javac -classpath $MYCLASSPATH $PROGRAM
echo done

