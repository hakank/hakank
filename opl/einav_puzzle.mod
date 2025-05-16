/* 

  A programming puzzle from Einav in OPL.

  From 
  "A programming puzzle from Einav"
  http://gcanyon.wordpress.com/2009/10/28/a-programming-puzzle-from-einav/
  """
  My friend Einav gave me this programming puzzle to work on. Given this array of positive and negative numbers:
  33   30  -10 -6  18   7  -11 -23   6
  ...
  -25   4  16  30  33 -23  -4   4 -23

  You can flip the sign of entire rows and columns, as many of them
  as you like. The goal is to make all the rows and columns sum to positive
  numbers (or zero), and then to find the solution (there are more than one)
  that has the smallest overall sum. So for example, for this array:
  33  30 -10
  -16  19   9
  -17 -12 -14
  You could flip the sign for the bottom row to get this array:
  33  30 -10
  -16  19   9
  17  12  14
  Now all the rows and columns have positive sums, and the overall total is 
  108.
  But you could instead flip the second and third columns, and the second 
  row, to get this array:
  33  -30  10
  16   19    9
  -17   12   14
  All the rows and columns still total positive, and the overall sum is just 
  66. So this solution is better (I don’t know if it’s the best)
  A pure brute force solution would have to try over 30 billion solutions. 
  I wrote code to solve this in J. I’ll post that separately.
  """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int rows = 27;
int cols = 9;
int data[1..rows, 1..cols] = 
[
[33,30,10,-6,18,-7,-11,23,-6],
[16,-19,9,-26,-8,-19,-8,-21,-14],
[17,12,-14,31,-30,13,-13,19,16],
[-6,-11,1,17,-12,-4,-7,14,-21],
[18,-31,34,-22,17,-19,20,24,6],
[33,-18,17,-15,31,-5,3,27,-3],
[-18,-20,-18,31,6,4,-2,-12,24],
[27,14,4,-29,-3,5,-29,8,-12],
[-15,-7,-23,23,-9,-8,6,8,-12],
[33,-23,-19,-4,-8,-7,11,-12,31],
[-20,19,-15,-30,11,32,7,14,-5],
[-23,18,-32,-2,-31,-7,8,24,16],
[32,-4,-10,-14,-6,-1,0,23,23],
[25,0,-23,22,12,28,-27,15,4],
[-30,-13,-16,-3,-3,-32,-3,27,-31],
[22,1,26,4,-2,-13,26,17,14],
[-9,-18,3,-20,-27,-32,-11,27,13],
[-17,33,-7,19,-32,13,-31,-2,-24],
[-31,27,-31,-29,15,2,29,-15,33],
[-18,-23,15,28,0,30,-4,12,-32],
[-3,34,27,-25,-18,26,1,34,26],
[-21,-31,-10,-13,-30,-17,-12,-26,31],
[23,-31,-19,21,-17,-10,2,-23,23],
[-3,6,0,-3,-32,0,-10,-25,14],
[-19,9,14,-27,20,15,-5,-27,18],
[11,-6,24,7,-17,26,20,-31,-25],
[-25,4,-16,30,33,23,-4,-4,23]
];


// decision variables
dvar int x[1..rows, 1..cols] in -100..100;

// row/column sums
dvar int row_sums[1..rows] in 0..200;
dvar int col_sums[1..cols] in 0..200;

// the signs of rows and column
dvar int row_signs[1..rows] in -1..1; // {-1,1}
dvar int col_signs[1..cols] in -1..1; // {-1,1}

// total sum (to minimize)
dvar int total_sum in 0..1000;



execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.SearchType = "MultiPoint";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.DefaultInferenceLevel="Extended"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  /*
  var f = cp.factory;
  var s = f.searchPhase(x,
                        // variable
                        f.selectSmallest(f.domainMin()),

                        // value
                        f.selectSmallest(f.value())
                       );

  cp.setSearchPhases(s);
  */
 
}


minimize total_sum;
constraints {

  // fix the domains of the signs; should be {-1,1}.
  forall(i in 1..rows) row_signs[i] != 0;
  forall(i in 1..cols) col_signs[i] != 0;

  forall(i in 1..rows, j in 1..cols) {
    x[i,j] == data[i,j]*row_signs[i]*col_signs[j];
  }

  total_sum == sum(i in 1..rows, j in 1..cols) x[i,j];

  forall(i in 1..rows) {
    row_sums[i] == sum(j in 1..cols) row_signs[i]*col_signs[j]*data[i,j];
  }

  forall(j in 1..cols) {
    col_sums[j] == sum(i in 1..rows) row_signs[i]*col_signs[j]*data[i,j];
  }

  // total_sum <= 812; 


}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("x     :", t.x);
        writeln("row_sums :", t.row_sums);
        writeln("col_sums :", t.col_sums);
        writeln("row_signs :", t.row_signs);
        writeln("col_signs :", t.col_signs);
        writeln("total_sum :", t.total_sum);
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
