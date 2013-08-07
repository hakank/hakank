#!/bin/sh

#
# Compile a JSR331 program
#
# 2010-10-04/hakank@bonetmail.com

PROBLEM=$1
THIS_SOLVER=$2

echo "Problem:$PROBLEM Solver: $THIS_SOLVER"

if [ ! -e "$PROBLEM" ]; then
   echo "File $PROBLEM do not exist."
   exit;
fi

# Solvers
CONSTRAINER_SOLVER=./lib/constrainer/jsr331.constrainer.jar:./lib/constrainer/constrainer.light.jar
# CHOCO_SOLVER=./lib/choco/jsr331.choco.jar:./lib/choco/choco-solver-2.1.1-20100709.142532-2.jar
CHOCO_SOLVER=./lib/choco/jsr331.choco.jar:./lib/choco/choco-solver-2.1.1-20110622-with-sources.jar

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

# export PROGRAM=org.jcp.jsr331.samples.$1
PROGRAM=$PROBLEM
echo Run $PROGRAM $THIS_SOLVER
export LOGLIBS=./lib/logging/commons-logging-1.1.jar:./lib/logging/commons-logging-api-1.1.jar:./lib/logging/log4j-1.2.15.jar

export LIBS=./bin:./lib/jsr331.jar:$SOLVER:$LOGLIBS
echo java -Xmx512m -classpath "$LIBS" $PROGRAM
javac -classpath "$LIBS" $PROGRAM
echo done

