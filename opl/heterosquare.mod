/* 

  Heterosquare in OPL.

  From http://willow.engr.uconn.edu/cometPubWiki/index.php/Heterosquare
  """
  A heterosquare of order n is a n*n square whose elements are distinct integers from 
  1 to n^2 such that the sums of the rows, columns and diagonals are all different. 
  Here is an example of heterosquare of order 3 
  
             19
  
  1  2  3    6
  8  9  4    21
  7  6  5    18
  
  16 17 12   15  (Sums)
  """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 3;

// decision variables
dvar int x[1..n, 1..n] in 1..n*n;
dvar int row_sums[1..n] in 1..n*n*n;
dvar int col_sums[1..n] in 1..n*n*n;
dvar int diag1 in 1..n*n*n;
dvar int diag2 in 1..n*n*n;
dvar int allsums[1..n*2+2] in 1..n*n*n; 

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

   // all the entries in the matrix should be different
   allDifferent(x); 

   // and all sums should be different
   // allDifferent(row_sums ++ col_sums ++ [diag1, diag2]);
   forall(i in 1..n) {
      allsums[i] == row_sums[i];
      allsums[i+n] == col_sums[i];
   }
   allsums[2*n+1] == diag1;
   allsums[2*n+2] == diag2;

   allDifferent(allsums);

   // calculate rows sums
   forall(i in 1..n) {
        sum(j in 1..n) x[i,j] == row_sums[i];
   }

   // calculate column sums
   forall(j in 1..n) {
       sum(i in 1..n) x[i,j] == col_sums[j];
   }
   // diag1 sums
   sum(i in 1..n) x[i,i] == diag1;
   // diag2 sums
   sum(i in 1..n) x[i,n-i+1] == diag2;

   //
   // symmetry breaking
   //
   // From http://en.wikipedia.org/wiki/Heterosquare
   // """
   // It is strongly suspected that there are exactly 3120 
   // essentially different heterosquares of order 3.
   // """
   //
   // From
   // http://en.wikipedia.org/wiki/Fr%C3%A9nicle_standard_form
   // """
   // A magic square is in Frénicle standard form, named for 
   // Bernard Frénicle de Bessy, if the following two conditions apply:
   //  - the element at position [1,1] (top left corner) is the smallest 
   //    of the four corner elements; and
   //  - the element at position [1,2] (top edge, second from left) is 
   //    smaller than the element in [2,1].
   // """
   // (Note: For n=3 this gives 3120 solutions, as suspected...)
   // 
   x[1,1] < x[1,n]; 
   x[1,1] < x[n,1]; 
   x[1,1] < x[n,n];
   x[1,2] < x[2,1];


}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("x:     ", t.x);
        writeln("row_sums:", t.row_sums);
        writeln("col_sums:", t.col_sums);
        writeln("diag1   : ", t.diag1);
        writeln("diag2   : ", t.diag2);
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
