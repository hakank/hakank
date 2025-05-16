/* 

  Assignment problem in OPL.

  Winston "Operations Research", Assignment Problems, page 393f

  (generalized version with added test column)

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int rows = 4; // number of tasks
int cols = 5; // number of persons

// added the fifth column (person)
int cost[1..rows, 1..cols] = 
[
 [14,  5, 8,  7, 15],
 [ 2, 12, 6,  5,  3],
 [ 7,  8, 3,  9,  7],
 [ 2,  4, 6, 10,  1]
];


// decision variables
dvar int x[1..rows, 1..cols] in 0..1;
dvar int total_cost;
dvar int a[1..rows] in 1..cols; // who is assigned

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

minimize total_cost;
constraints {

  total_cost == sum(i in 1..rows, j in 1..cols) x[i,j]*cost[i,j];
 
  // exacly one assignment per row (task), all tasks must be assigned
  forall(i in 1..rows) {
      sum(j in 1..cols) x[i,j] == 1;
  }


  // zero or one assignments per column (person)
  forall(j in 1..cols) {
        sum(i in 1..rows) x[i,j] <= 1;
  }

  // who is assigned to the task?
  forall(i in 1..rows) {
     a[i] == sum(j in 1..cols) j*x[i,j];
  }

}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("\nx:", t.x);
        writeln("a:", t.a);
        writeln("total_cost:", t.total_cost);
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
