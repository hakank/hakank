/* 

  Sudoku solver in OPL.



  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int s = 3;
int n = s*s;

// int board[1..n, 1..n] = ...;

// From "The chaos of sudoku"
// http://plus.maths.org/content/chaos-sudoku
// "...one of the hardest Sudoku puzzles on record"

// 
int board[1..n, 1..n] = 
  [
    [3,7,0, 6,0,0, 0,0,0],
    [0,0,9, 0,0,0, 0,0,0],
    [0,6,0, 0,2,0, 1,8,0],

    [0,0,0, 0,0,5, 0,0,0],
    [0,2,0, 0,1,0, 0,9,0],
    [0,0,0, 4,0,0, 0,0,0],

    [0,1,6, 0,9,0, 0,7,0],
    [0,0,0, 0,0,0, 5,0,0],
    [0,0,0, 0,0,7, 0,4,2]
  ];


// decision variables
dvar int x[1..n, 1..n] in 1..n;



execute {
  cp.param.SearchType = "DepthFirst";
  cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  cp.param.LogPeriod = 1;
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

    // the given hints
    forall(i in 1..n, j in 1..n: board[i,j] > 0) {
       x[i,j] == board[i,j];
    }

    // latin square
    forall(i in 1..n) {
      // rows
      allDifferent(all(j in 1..n) x[i,j]);
      // columns
      allDifferent(all(j in 1..n) x[j,i]);
    }

    // cells
    forall(i in 0..s-1,j in 0..s-1) {
       allDifferent(all(r in i*s+1..i*s+s, c in j*s+1..j*s+s) x[r,c]);
    } 

    /* 
    // cells: alternative approach
    forall(a, o, a1, o1, a2, o2 in 1..s:
     		          a1 != a2 && o1 != o2) {
		x[(a - 1) * s + a1, (o - 1) * s + o1] !=
                x[(a - 1) * s + a2, (o - 1) * s + o2];
    }
    */


}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      var x = thisOplModel.x;
      var n = thisOplModel.n;
      // writeln("x: ", x);
      for(var i = 1; i <= n; i++) {
        write("  ");
        for(var j = 1; j <= n; j++) {
           write(x[i][j], " ");
        }
        writeln();
      }

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
