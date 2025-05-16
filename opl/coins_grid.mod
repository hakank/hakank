/* 

  Coins puzzle in OPL.

  Problem from 
  Tony Hurlimann: "A coin puzzle - SVOR-contest 2007"
  http://www.svor.ch/competitions/competition2007/AsroContestSolution.pdf
  """
  In a quadratic grid (or a larger chessboard) with 31x31 cells, one should place coins in such a
  way that the following conditions are fulfilled:
     1. In each row exactly 14 coins must be placed.
     2. In each column exactly 14 coins must be placed.
     3. The sum of the quadratic horizontal distance from the main diagonal of all cells
        containing a coin must be as small as possible.
     4. In each cell at most one coin can be placed.
  The description says to place 14x31 = 434 coins on the chessboard each row containing 14
  coins and each column also containing 14 coins.
  """

  Cf the LPL model:
  http://diuflx71.unifr.ch/lpl/GetModel?name=/puzzles/coin

  Note: This is solved by an MIP solver in no-time, but CP solver has a hard
        time on it, and this is why I include it in my first testing models.

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 31; // = 31; // the grid size
int c = 14; // = 14; // number of coins per row/column

// decision variables
dvar int x[1..n,1..n] in 0..1; // the grid
dvar int z in 0..n*c*n*c;


execute {
  cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
  // cp.param.SearchType = "DepthFirst"; // "DepthFirst", "Restart", "MultiPoint"
  cp.param.SearchType = "Restart";
  cp.param.RestartGrowthFactor = 1.5; // Default 1.05
  // cp.param.AllDiffInferenceLevel = "Low"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 4;
 
  var f = cp.factory;
  var s = f.searchPhase(x,
                        // variable
                        // f.selectSmallest(f.domainSize()),
                        // f.selectLargest(f.impact()),
                        // f.selectLargest(f.regretOnMax()), // <-
                        f.selectLargest(f.domainMax()),

                        // value
                        // f.selectSmallest(f.value())
                        // f.selectLargest(f.value()) // <-
                        f.selectLargest(f.value())
                       );
  cp.setSearchPhases(s);

}

minimize z;
constraints {

  forall(i in 1..n) {
    // rows
    sum(j in 1..n) x[i,j] == c;
    // columns
    sum(j in 1..n) x[j,i] == c;
  }

  //  quadratic horizonal distance
  z == sum(i,j in 1..n) x[i,j]*(abs(i-j))*(abs(i-j));

}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      writeln();
      var n = thisOplModel.n;
      var x = thisOplModel.x;
      var z = thisOplModel.z;
      writeln("z: ", z);
      for(var i = 1; i <= n; i++) {
        write("  ");
        for(var j = 1; j <= n; j++) {
           write(x[i][j], " ");
        }
        writeln();
      }

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
