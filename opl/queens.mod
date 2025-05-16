/* 

  n-queens problem in OPL.

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

// int n = ...;
int n = 1000;
range dom = 1..n;

dvar int q1[dom] in dom;
dvar int q2[dom] in 1..n*2;
dvar int q3[dom] in -n..n;

execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Low"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  //var f = cp.factory;
  //cp.setSearchPhases(f.searchPhase(q1));

  var f = cp.factory;
  var s = f.searchPhase(q1,
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


  forall(i in dom) {
     q2[i] == q1[i]+i;
     q3[i] == q1[i]-i;
  };

  // This don't work since all() don't handle
  // expressions:
  // allDifferent(all(i in dom) (q[i]+i));

  allDifferent(q1);
  allDifferent(q2);
  allDifferent(q3);

}

/*
execute 
{
  writeln(q1);   
}
*/


main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      writeln("q1: ", thisOplModel.q1);
      // writeln("q2: ", thisOplModel.q2);
      // writeln("q3: ", thisOplModel.q3);
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
      if (thisOplModel.n > 10 && sol >= 1) {
        break;
      }
   }

   cp.endSearch();
   writeln("#solutions: ", sol);


} 
