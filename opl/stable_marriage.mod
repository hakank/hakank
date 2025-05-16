/* 

  Stable marriage problem in OPL.

  Based on the OPL version from
  Pascal Van Hentenryck "The OPL Optimization Programming Language"
  Also see
  http://www.comp.rgu.ac.uk/staff/ha/ZCSP/additional_problems/stable_marriage/stable_marriage.pdf


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = ...;
int rankWomen[1..n, 1..n] = ...;
int rankMen[1..n, 1..n] = ...;

// data from van Hentenryck (OPL book)
/*
int n = 5;
int rankWomen[1..n, 1..n] = 
 [[1, 2, 4, 3, 5],
  [3, 5, 1, 2, 4],
  [5, 4, 2, 1, 3],
  [1, 3, 5, 4, 2],
  [4, 2, 3, 5, 1]
 ];

int rankMen[1..n, 1..n] =
 [[5, 1, 2, 4, 3],
  [4, 1, 3, 2, 5],
  [5, 3, 2, 4, 1],
  [1, 5, 4, 3, 2],
  [4, 3, 2, 1, 5]
 ];
*/
// decision variables
dvar int wife[1..n] in 1..n; 
dvar int husband[1..n] in 1..n; 

execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Low"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s = f.searchPhase(wife,
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

   forall(m in 1..n) {
       husband[wife[m]] == m;
   }

   forall(w in 1..n) {
      wife[husband[w]] == w; 
   }

   forall(m in 1..n, o in 1..n) {
      rankMen[m,o] < rankMen[m, wife[m]] =>
      rankWomen[o,husband[o]] < rankWomen[o,m];
   }

   forall(w in 1..n, o in 1..n) {
      rankWomen[w,o] < rankWomen[w,husband[w]] =>
      rankMen[o,wife[o]] < rankMen[o,w];
   }

   // Redundant constraints
   allDifferent(wife);
   allDifferent(husband);
   inverse(wife,husband);
}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      writeln("wife   : ", thisOplModel.wife);
      writeln("husband: ", thisOplModel.husband);
      writeln();

      sol++;
   }

   cp.endSearch();
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
