/* 

  Talisman square in OPL.

  http://mathworld.wolfram.com/TalismanSquare.html
  """
  An n×n array  of the integers from 1 to n^2 such that the difference between 
  any one integer and its neighbor (horizontally, vertically, or diagonally, 
  without wrapping around) is greater than or equal to some value k is 
  called a (n,k)-talisman square. 
  """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 5;
int k = 2;

// decision variables
dvar int x[1..n, 1..n] in 1..n*n;

execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.DefaultInferenceLevel="Extended"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s = f.searchPhase(x,
                        // variable
                        f.selectSmallest(f.domainMin()),

                        // value
                        f.selectSmallest(f.value())
                       );

  cp.setSearchPhases(s);

 
}

constraints {

   allDifferent(x); 

   forall(i, j in 2..n) {
     abs(x[i,j]-x[i-1,j]) >= k;
     abs(x[i,j]-x[i,j-1]) >= k;
   }

   forall(i, j in 1..n-1) {
     abs(x[i,j]-x[i+1,j]) >= k;
     abs(x[i,j]-x[i,j+1]) >= k;
   }

   // symmetry breaking
   // x[1,1] == 1;

}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("x:     ", t.x);
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
