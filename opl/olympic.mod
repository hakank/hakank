/* 

  Olympic puzzle in OPL.

  Benchmark for Prolog (BProlog)
  """
  File   : olympic.pl
  Author : Neng-Fa ZHOU
  Date   : 1993

  Purpose: solve a puzzle taken from Olympic Arithmetic Contest

   Given ten variables with the following configuration:

               X7   X8   X9   X10
                  X4   X5   X6
                     X2   X1             
                        X1

  We already know that X1 is equal to 3 and want to assign each variable
  with a different integer from {1,2,...,10} such that for any three
  variables 
                      Xi   Xj

                         Xk
  the following constraint is satisfied:

                    |Xi-Xj| = Xk
  """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 10;
range r = 1..n;


// decision variables
dvar int X1 in r;
dvar int X2 in r;
dvar int X3 in r;
dvar int X4 in r;
dvar int X5 in r;
dvar int X6 in r;
dvar int X7 in r;
dvar int X8 in r;
dvar int X9 in r;
dvar int X10 in r;
dvar int x[r] = [X1,X2,X3,X4,X5,X6,X7,X8,X9,X10];

execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  
  var f = cp.factory;
  var s = f.searchPhase(x,
                        // variable
                        f.selectSmallest(f.domainMin()),

                        // value
                        f.selectSmallest(f.value())
                       );
  cp.setSearchPhases(s);

}



constraints {

    allDifferent(x);
    X1 == 3;

    abs(X2-X3) == X1;
    abs(X4-X5) == X2;
    abs(X5-X6) == X3;
    abs(X7-X8) == X4;
    abs(X8-X9) == X5;
    abs(X9-X10) == X6;

}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("x  :", t.x);
        writeln();
        sol++;
     }
     cp.endSearch();

     writeln();
     var info = cp.info;
     writeln("#ModelVariables: ", info.NumberOfModelVariables);
     writeln("#Variables: ", info.NumberOfVariables);
     writeln("#Constraints: ", info.NumberOfConstraints);
     writeln("#Choice points: ", info.NumberOfChoicePoints);
     writeln("#Fails: ", info.NumberOfFails);
     writeln("#Branches: ", info.NumberOfBranches);
     writeln("TotalTime: ", info.TotalTime);
     writeln("SolveTime: ", info.SolveTime);
     writeln();

     writeln("#solutions: ", sol);

} 
