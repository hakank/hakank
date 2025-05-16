/* 

  Place number puzzle in OPL.

  http://ai.uwaterloo.ca/~vanbeek/Courses/Slides/introduction.pdf
  """
  Place numbers 1 through 8 on nodes
  - each number appears exactly once
  - no connected nodes have consecutive numbers
       2 - 5 
     / | X | \
   1 - 3 - 6 - 8
     \ | X | /
       4 - 7
  """
  
  Note: This is the same problem as NumberEightPuzzle.mzn


  Two solutions (plus their reverses):
  x = [2, 5, 8, 6, 3, 1, 4, 7]  
  x = [2, 6, 8, 5, 4, 1, 3, 7]


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 8;
int m = 32;
int graph[1..m, 1..2] = 
[
  [1,2],
  [1,3],
  [1,4],
  [2,1],
  [2,3],
  [2,5],
  [2,6],
  [3,2],
  [3,4],
  [3,6],
  [3,7],
  [4,1],
  [4,3],
  [4,6],
  [4,7],
  [5,2],
  [5,3],
  [5,6],
  [5,8],
  [6,2],
  [6,3],
  [6,4],
  [6,5],
  [6,7],
  [6,8],
  [7,3],
  [7,4],
  [7,6],
  [7,8],
  [8,5],
  [8,6],
  [8,7]
];

// decision variables
dvar int x[1..n] in 1..n; 


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
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
  forall(i in 1..m) {
     abs(x[graph[i,1]]-x[graph[i,2]]) > 1 ;
  }

  //symmetry breaking
  x[1] < x[n];

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
