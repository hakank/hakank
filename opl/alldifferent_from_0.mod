/* 

  Decomposition of global constraint alldifferent_except_0 in OPL.

  From Global constraint catalogue:
  http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_except_0.html
  """ 
  Enforce all variables of the collection VARIABLES to take distinct 
  values, except those variables that are assigned to 0.
  
  Example
     (<5, 0, 1, 9, 0, 3>)
  
  The alldifferent_except_0 constraint holds since all the values 
  (that are different from 0) 5, 1, 9 and 3 are distinct.
  """


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 6;

dvar int x[1..n] in 0..n;

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
                       );
  cp.setSearchPhases(s);

}

constraints {

   // alldifferent_except_0 (decomposition)
   forall(ordered i,j in 1..n) {
      x[i] > 0 && x[j] > 0 => x[i] != x[j];
   }

   // increasing (just to get some less solutions)
   forall(i in 2..n) {
      x[i-1] <= x[i];
   }
}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      writeln("x: ", thisOplModel.x);
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
