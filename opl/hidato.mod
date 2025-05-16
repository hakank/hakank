/* 

  Hidato puzzle in OPL.

  http://www.shockwave.com/gamelanding/hidato.jsp
  http://www.hidato.com/
  """
  Puzzles start semi-filled with numbered tiles.
  The first and last numbers are circled.
  Connect the numbers together to win. Consecutive
  number must touch horizontally, vertically, or
  diagonally.
  """


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

// Simple problem
/*
int r = 3;
int c = r;
int puzzle[1..r, 1..c] =
[ 
  [6,0,9],
  [0,2,8],
  [1,0,0]
];
*/

// Problem from the book:
// Gyora Bededek: "Hidato: 2000 Pure Logic Puzzles"
// problem 1 (Practice)
/*
int r = 5;
int c = r;
int puzzle[1..r, 1..c] = 
[
    [ 0, 0,20, 0, 0],
    [ 0, 0, 0,16,18],
    [22, 0,15, 0, 0],
    [23, 0, 1,14,11],
    [ 0,25, 0, 0,12]
];
*/

int r = 7;
int c = r;
int puzzle[1..r, 1..c] = 
  [ 
  [ 0,44,41, 0, 0, 0, 0],
  [ 0,43, 0,28,29, 0, 0],
  [ 0, 1, 0, 0, 0,33, 0],
  [ 0, 2,25, 4,34, 0,36],
  [49,16, 0,23, 0, 0, 0],
  [ 0,19, 0, 0,12, 7, 0],
  [ 0, 0, 0,14, 0, 0, 0] 
];


/*
  Problem 156 (Master)
  (This seems to be harder to solve than the 12x12 prolem 188 below...)
*/
/*
int r = 10;
int c = r;
int puzzle[1..r, 1..c] = 
  [
   [88, 0,0,100, 0, 0,37, 0, 0,34],
   [ 0,86, 0,96,41, 0, 0,36, 0, 0],
   [ 0,93,95,83, 0, 0, 0,31,47, 0],
   [ 0,91, 0, 0, 0, 0, 0,29, 0, 0],
   [ 11,0, 0, 0, 0, 0, 0,45,51, 0],
   [ 0, 9, 5, 3, 1, 0, 0, 0, 0, 0],
   [ 0,13, 4, 0, 0, 0, 0, 0, 0, 0],
   [15, 0, 0,25, 0, 0,54,67, 0, 0],
   [ 0,17, 0,23, 0,60,59, 0,69, 0],
   [19, 0,21,62,63, 0, 0, 0, 0, 0]
  ];
*/


/*
  Problem 188 (Genius)
*/
/*
int r = 12;
int c = r;
int puzzle[1..r, 1..c] = 
 [
   [  0,  0,134,  2,  4,  0,  0,  0,  0,  0,  0,  0],
   [136,  0,  0,  1,  0,  5,  6, 10,115,106,  0,  0],
   [139,  0,  0,124,  0,122,117,  0,  0,107,  0,  0],
   [  0,131,126,  0,123,  0,  0, 12,  0,  0,  0,103],
   [  0,  0,144,  0,  0,  0,  0,  0, 14,  0, 99,101],
   [  0,  0,129,  0, 23, 21,  0, 16, 65, 97, 96,  0],
   [ 30, 29, 25,  0,  0, 19,  0,  0,  0, 66, 94,  0],
   [ 32,  0,  0, 27, 57, 59, 60,  0,  0,  0,  0, 92],
   [  0, 40, 42,  0, 56, 58,  0,  0, 72,  0,  0,  0],
   [  0, 39,  0,  0,  0,  0, 78, 73, 71, 85, 69,  0],
   [ 35,  0,  0, 46, 53,  0,  0,  0, 80, 84,  0,  0],
   [ 36,  0, 45,  0,  0, 52, 51,  0,  0,  0,  0, 88]
 ];
*/


range ab = -1..1;

// decision variables
dvar int x[1..r, 1..c] in 1..r*c;

dvar int itmp[1..r*c-1] in 1..r;
dvar int jtmp[1..r*c-1] in 1..r;
dvar int atmp[1..r*c-1] in ab;
dvar int btmp[1..r*c-1] in ab;


execute {
  // cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.DefaultInferenceLevel="Extended"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  /*
  var f = cp.factory;
  var s1 = f.searchPhase(x,
                        // variable
                        f.selectSmallest(f.domainSize()),
                        // f.selectSmallest(f.impact()),

                        // value
                        // f.selectSmallest(f.value())
                        f.selectSmallest(f.value())
                       );

  var s2 = f.searchPhase(itmp,
                        // variable
                        f.selectLargest(f.domainSize()),

                        // value
                        // f.selectSmallest(f.value())
                        f.selectLargest(f.value())
                       );

  var s3 = f.searchPhase(jtmp,
                        // variable
                        f.selectLargest(f.domainSize()),

                        // value
                        // f.selectSmallest(f.value())
                        f.selectLargest(f.value())
                       );

  cp.setSearchPhases(s1,s2,s3);
  */
}


constraints {

  // place all integers from 1..r*c
  allDifferent(x);

  // place the fixed tiles
  forall(i in 1..r, j in 1..c: puzzle[i,j] > 0) {
         x[i,j] == puzzle[i,j];
  }

  forall(k in 1..r*c-1) {
        k == x[itmp[k], jtmp[k]]; // fix this k

        // find the next k
        itmp[k]+atmp[k] >= 1 && jtmp[k]+btmp[k] >=  1 &&
        itmp[k]+atmp[k] <= r && jtmp[k]+btmp[k] <= c  && 
        // abs(atmp[k]) + abs(btmp[k]) >= 1 &&
        !(atmp[k] == 0 && btmp[k] == 0) &&
        k + 1 == x[itmp[k]+atmp[k], jtmp[k]+btmp[k]];
  }

}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   writeln();
   while (cp.next()) {
      var t = thisOplModel;
      // writeln("x:     ", t.x);
      for(i = 1; i <= t.r; i++) {
         for(j = 1; j <= t.c; j++) {
            if (t.x[i][j] < 10) {
               write(" ");
            }
            write(t.x[i][j], " ");
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
