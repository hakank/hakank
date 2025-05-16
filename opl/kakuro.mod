/* 

  Kakuro puzzle in OPL.

  http://en.wikipedia.org/wiki/Kakuro
  """
  The object of the puzzle is to insert a digit from 1 to 9 inclusive 
  into each white cell such that the sum of the numbers in each entry 
  matches the clue associated with it and that no digit is duplicated in 
  any entry. It is that lack of duplication that makes creating Kakuro 
  puzzles with unique solutions possible, and which means solving a Kakuro 
  puzzle involves investigating combinations more, compared to Sudoku in 
  which the focus is on permutations. There is an unwritten rule for 
  making Kakuro puzzles that each clue must have at least two numbers 
  that add up to it. This is because including one number is mathematically 
  trivial when solving Kakuro puzzles; one can simply disregard the 
  number entirely and subtract it from the clue it indicates.
  """

  This model solves the problem at the Wikipedia page. 
  For a larger picture, see
  http://en.wikipedia.org/wiki/File:Kakuro_black_box.svg

  The solution:
    9 7 0 0 8 7 9
    8 9 0 8 9 5 7
    6 8 5 9 7 0 0
    0 6 1 0 2 6 0
    0 0 4 6 1 3 2
    8 9 3 1 0 1 4
    3 1 2 0 0 2 1

  or rather

    9 7 _ _ 8 7 9
    8 9 _ 8 9 5 7
    6 8 5 9 7 _ _
    _ 6 1 _ 2 6 _
    _ _ 4 6 1 3 2
    8 9 3 1 _ 1 4
    3 1 2 _ _ 2 1


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 7;

//
// state the problem (without the operation)
//
// For a better view of the problem, see
//  http://en.wikipedia.org/wiki/File:KenKenProblem.svg
//
int num_p = 24; // number of segments
int num_hints = 5;  // number of hints per segments (that's max number of hints)
int max_val = 100;
int P[1..num_p, 1..2*num_hints+1] = 
[
   [1,1, 1,2, 0,0, 0,0, 0,0,  16],
   [1,5, 1,6, 1,7, 0,0, 0,0,  24],
   [2,1, 2,2, 0,0, 0,0, 0,0,  17],
   [2,4, 2,5, 2,6, 2,7, 0,0,  29],
   [3,1, 3,2, 3,3, 3,4, 3,5,  35],
   [4,2, 4,3, 0,0, 0,0, 0,0,   7], 
   [4,5, 4,6, 0,0, 0,0, 0,0,   8], 
   [5,3, 5,4, 5,5, 5,6, 5,7,  16], 
   [6,1, 6,2, 6,3, 6,4, 0,0,  21], 
   [6,6, 6,7, 0,0, 0,0, 0,0,   5], 
   [7,1, 7,2, 7,3, 0,0, 0,0,   6], 
   [7,6, 7,7, 0,0, 0,0, 0,0,   3], 

   [1,1, 2,1, 3,1, 0,0, 0,0,  23], 
   [1,2, 2,2, 3,2, 4,2, 0,0,  30], 
   [1,5, 2,5, 3,5, 4,5, 5,5,  27], 
   [1,6, 2,6, 0,0, 0,0, 0,0,  12], 
   [1,7, 2,7, 0,0, 0,0, 0,0,  16], 
   [2,4, 3,4, 0,0, 0,0, 0,0,  17],    
   [3,3, 4,3, 5,3, 6,3, 7,3,  15], 
   [4,6, 5,6, 6,6, 7,6, 0,0,  12], 
   [5,4, 6,4, 0,0, 0,0, 0,0,   7],    
   [5,7, 6,7, 7,7, 0,0, 0,0,   7], 
   [6,1, 7,1, 0,0, 0,0, 0,0,  11], 
   [6,2, 7,2, 0,0, 0,0, 0,0,  10]
];


//
// the blanks, coded as 0 in x
//
int num_blanks = 13;
int max_blanks_cells = 3;
int blanks[1..2*num_blanks] = 
[
   1,3, 1,4,
   2,3,
   3,6, 3,7,
   4,1, 4,4, 4,7,
   5,1, 5,2,
   6,5,
   7,4, 7,5
];



// decision variables
dvar int x[1..n, 1..n] in 0..9;


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

  forall(i in 1..num_blanks) {
     x[ blanks[2*(i-1)+1], blanks[2*(i-1)+2]] == 0;
  }

  forall(p in 1..num_p) {
    (sum(i in 1..num_hints: P[p,2*(i-1)+1] > 0) (x[P[p, 2*(i-1)+1], P[p,2*(i-1)+2]]) == P[p, 2*num_hints+1]);
    
    allDifferent(all(i in 1..num_hints: P[p,2*(i-1)+1] > 0) 
        x[P[p, 2*(i-1)+1], P[p, 2*(i-1)+2]]
    );

   forall(i in 1..num_hints: P[p,2*(i-1)+1] > 0) {
        x[P[p, 2*(i-1)+1], P[p, 2*(i-1)+2]] > 0;
   }
  }

}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;
     var alpha = " abcdefghijklmnopqrstuvwy";
     while (cp.next()) {
        var t = thisOplModel;
        for(i = 1; i <= t.n; i++) {
            for(j = 1; j <= t.n; j++) {
              var v = t.x[i][j];
              if (v > 0) {
                write(v, " ");  
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
