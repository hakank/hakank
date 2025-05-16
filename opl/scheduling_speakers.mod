/* 

  Scheduling speakers in OPL.

  From Rina Dechter, Constraint Processing, page 72
  Scheduling of 6 speakers in 6 slots.

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 6;

//  slots available to speak
{int} available[1..n] = [
               // Reasoning:             
 {3,4,5,6},    // 2) the only one with 6 after speaker F -> 1
 {3,4},        // 5) 3 or 4
 {2,3,4,5},    // 3) only with 5 after F -> 1 and A -> 6
 {2,3,4},      // 4) only with 2 after C -> 5 and F -> 1 
 {3,4},        // 5) 3 or 4
 {1,2,3,4,5,6} // 1) the only with 1
];


// decision variables
dvar int x[1..n] in 1..n; // the allotted speakers


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

   allDifferent(x);

   forall(i in 1..n) {
      x[i] in available[i];
   }
}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("x:", t.x);
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
