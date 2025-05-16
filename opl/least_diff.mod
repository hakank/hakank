/* 

  Least diff problem in OPL.

  The model solves the following problem:
  
  What is the smallest difference between two numbers X - Y
  if you must use all the digits (0..9) exactly once, i.e.
  Minimize the difference 
    ABCDE - FGHIJ

  This problem was commented in (the Swedish) blogpost
  "Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc"
  http://www.hakank.org/webblogg/archives/001209.html


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 10;
range digits = 0..9;

dvar int A in digits;
dvar int B in digits;
dvar int C in digits;
dvar int D in digits;
dvar int E in digits;

dvar int F in digits;
dvar int G in digits;
dvar int H in digits;
dvar int I in digits;
dvar int J in digits;

dvar int fd[1..n] = [A,B,C,D,E,F,G,H,I,J];

dvar int X in 0..99999;
dvar int Y in 0..99999;
dvar int Diff in 0..99999;

execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Low"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s = f.searchPhase(fd,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                        // f.selectLargest(f.value())
                        // f.selectRandomValue()
                       );
  cp.setSearchPhases(s);

}

minimize Diff;
constraints {

  allDifferent(fd);

  Diff == X - Y;
  Diff > 0;
  X == (10000*A +1000*B +100*C +10*D + E);
  Y == (10000*F +1000*G +100*H +10*I + J);
  

}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      writeln("Diff: ", thisOplModel.Diff);

      writeln(thisOplModel.printSolution());
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
      sol++;
   }

   cp.endSearch();
   writeln("#solutions: ", sol);

} 
