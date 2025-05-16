/* 

  SEND+MORE=MONEY problem in OPL.

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int base = 14;
int n = 8;
range digits = 0..base-1;

dvar int S in digits;
dvar int E in digits;
dvar int N in digits;
dvar int D in digits;
dvar int M in digits;
dvar int O in digits;
dvar int R in digits;
dvar int Y in digits;

dvar int x[1..n] = [S,E,N,D,M,O,R,Y];

execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Low"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s = f.searchPhase(x,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                        // f.selectLargest(f.value())
                        // f.selectRandomValue()
                       );
  cp.setSearchPhases(s);

}

constraints {

  allDifferent(x);

            (base^3*S +base^2*E +base^1*N + D) +
            (base^3*M +base^2*O +base^1*R + E) ==
  (base^4*M +base^3*O +base^2*N +base^1*E + Y);
  
  S > 0;
  M > 0;

}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      writeln("x: ", thisOplModel.x);

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
