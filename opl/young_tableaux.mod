/* 

  Young tableaux and partitions in OPL.

  See 
  http://mathworld.wolfram.com/YoungTableau.html
  and
  http://en.wikipedia.org/wiki/Young_tableau
  """
  The partitions of 4 are
   {4}, {3,1}, {2,2}, {2,1,1}, {1,1,1,1}

  And the corresponding standard Young tableaux are:

  1.   1 2 3 4

  2.   1 2 3         1 2 4    1 3 4
       4             3        2

  3.   1 2           1 3
       3 4           2 4

  4    1 2           1 3      1 4 
       3             2        2 
       4             4        3

  5.   1
       2
       3
       4
  """  


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 6; 

// decision variables
dvar int x[1..n, 1..n] in 1..n+1;
dvar int p[1..n] in 0..n+1; // the partition structure


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

   // 1..n is used exactly once
   forall(i in 1..n) {
     count(x, i) == 1;
   }
 
   x[1,1] == 1;

   // row wise
   forall(i in 1..n) {
      forall(j in 2..n) {
        x[i,j] >= x[i,j-1];
      }
   }
   // column wise
   forall(j in 1..n) {
     forall(i in 2..n) {
       x[i,j] >= x[i-1,j];
     }
   }

   // calculate the structure (the partition)
   forall(i in 1..n) {
      p[i] == sum(j in 1..n) (x[i,j] <= n);
   }

   sum(i in 1..n) p[i] == n;

   forall(i in 2..n) {
      p[i-1] >= p[i];
   }
 
}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      var n = thisOplModel.n;
      var x = thisOplModel.x;
      var p = thisOplModel.p;
      writeln("\np: ", p);
      for(i = 1; i <= n; i++) {
         var c = 0;
         for(j = 1; j <= n; j++) {
            var v = x[i][j];
            if (v <= n) {
              write(v, " ");
              c++;
            }
         }
         if (c > 0) {
            writeln(); 
         }
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
