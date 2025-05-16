/* 

  Discrete tomography in OPL.

  Problem from http://eclipse.crosscoreop.com/examples/tomo.ecl.txt
  """
  This is a little "tomography" problem, taken from an old issue
  of Scientific American.

  A matrix which contains zeroes and ones gets "x-rayed" vertically and
  horizontally, giving the total number of ones in each row and column.
  The problem is to reconstruct the contents of the matrix from this
  information. Sample run:

  ?- go.
     0 0 7 1 6 3 4 5 2 7 0 0
  0                         
  0                         
  8      * * * * * * * *    
  2      *             *    
  6      *   * * * *   *    
  4      *   *     *   *    
  5      *   *   * *   *    
  3      *   *         *    
  7      *   * * * * * *    
  0                         
  0                         

  Eclipse solution by Joachim Schimpf, IC-Parc
  """


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

//
// These following three examples are from the ECLiPSe program cited above.
//
// int r = 11;
// int c = 12;
// int row_sums[1..r] = [0,0,8,2,6,4,5,3,7,0,0];
// int col_sums[1..c] = [0,0,7,1,6,3,4,5,2,7,0,0];
 
// int r = 5;
// int c = 13;
// int row_sums[1..r] = [10,4,8,5,6];
// int col_sums[1..c] = [5,3,4,0,5,0,5,2,2,0,1,5,1];


// This give three slightly different solutions.
// int r = 3;
// int c = 11;
// int row_sums[1..r] = [11,5,4];
// int col_sums[1..c] = [3,2,3,1,1,1,1,2,3,2,1];

// And this is my own
int r = 14;
int c = 14;
int row_sums[1..r] = [0,2,2,2,2,2,8,8,4,4,4,4,4,0];
int col_sums[1..c] = [0,0,0,12,12,2,2,2,2,7,7,0,0,0];


// decision variables
dvar int x[1..r, 1..c] in 0..1;


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Low"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.DefaultInferenceLevel="Extended"; // "Low", "Basic", "Medium", "Extended"
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
     // the rows
     forall(i in 1..r) {
       row_sums[i] == sum(j in 1..c) x[i,j];
     }

     // the columns
     forall(j in 1..c) {
       col_sums[j] == sum(i in 1..r) x[i,j];
     }


}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      var x = thisOplModel.x;
      for(i = 1; i <= thisOplModel.r; i++) {
         for(j = 1; j <= thisOplModel.c; j++) {
           if (x[i][j] == 1) {           
             write("# ");
           } else {
             write("_ ");
           }
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
