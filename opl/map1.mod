/* 

  Map coloring in OPL.

  Simple map coloring problem.

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 6; // number of countries
// belgium, denmark, france, germany, netherlands, luxembourg
int connections[1..n, 1..n] = 
  [    
     [0,   0,   1,   1,   1,   1],
     [0,   0,   0,   1,   0,   0],
     [1,   0,   0,   1,   1,   0],
     [1,   1,   1,   0,   1,   1],
     [1,   0,   1,   1,   0,   0],
     [1,   0,   0,   1,   0,   0]
  ];

// decision variables
dvar int color[1..n] in 1..4;


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Low"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s = f.searchPhase(color,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );
  cp.setSearchPhases(s);

}


constraints {

    forall(ordered i,j in 1..n: connections[i,j] == 1) {
       color[i] != color[j];
    }
 
    // symmetry breaking: belgium has color=1
    color[1] == 1;
}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      writeln("color: ", thisOplModel.color);
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
