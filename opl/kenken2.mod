/* 

  KenKen puzzle in OPL.

  http://en.wikipedia.org/wiki/KenKen
  """
  KenKen or KEN-KEN is a style of arithmetic and logical puzzle sharing
  several characteristics with sudoku. The name comes from Japanese and
  is translated as "square wisdom" or "cleverness squared".
  ...
  The objective is to fill the grid in with the digits 1 through 6 such that:

    * Each row contains exactly one of each digit
    * Each column contains exactly one of each digit
    * Each bold-outlined group of cells is a cage containing digits which
      achieve the specified result using the specified mathematical operation:
        addition (+),
        subtraction (-),
        multiplication (x),
        and division (÷).
        (Unlike in Killer sudoku, digits may repeat within a group.)

  ...
  More complex KenKen problems are formed using the principles described
  above but omitting the symbols +, -, x and ÷, thus leaving them as
  yet another unknown to be determined.
  """

  The solution is:

      5 6 3 4 1 2
      6 1 4 5 2 3
      4 5 2 3 6 1
      3 4 1 2 5 6
      2 3 6 1 4 5
      1 2 5 6 3 4


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 6;

//
// state the problem (without the operation)
//
// For a better view of the problem, see
//  http://en.wikipedia.org/wiki/File:KenKenProblem.svg
//
int num_p = 15; // number of segments
int num_hints = 4;  // number of hints per segments (that's max number of hints)
int P[1..num_p, 1..2*num_hints+1] = 
[
   [1,1, 2,1, 0,0, 0,0,  11],
   [1,2, 1,3, 0,0, 0,0,   2],
   [1,4, 2,4, 0,0, 0,0,  20],
   [1,5, 1,6, 2,6, 3,6,   6],
   [2,2, 2,3, 0,0, 0,0,   3],
   [2,5, 3,5, 0,0, 0,0,   3],
   [3,1, 3,2, 4,1, 4,2, 240],
   [3,3, 3,4, 0,0, 0,0,   6],
   [4,3, 5,3, 0,0, 0,0,   6],
   [4,4, 5,4, 5,5, 0,0,   7],
   [4,5, 4,6, 0,0, 0,0,  30],
   [5,1, 5,2, 0,0, 0,0,   6],
   [5,6, 6,6, 0,0, 0,0,   9],
   [6,1, 6,2, 6,3, 0,0,   8],
   [6,4, 6,5, 0,0, 0,0,   2]
];

// decision variables
dvar int x[1..n, 1..n] in 1..n;


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

  forall(i in 1..n) {
     allDifferent(all(j in 1..n) x[i,j]);
     allDifferent(all(j in 1..n) x[j,i]);
  }

  forall(p in 1..num_p) {
    if (sum(i in 1..2*num_hints) (P[p,i] > 0) == 4) {
       // is it just two numbers?
       (x[P[p,1], P[p,2]] + x[P[p,3], P[p,4]] == P[p, 2*num_hints+1]) ||
       (x[P[p,1], P[p,2]] * x[P[p,3], P[p,4]] == P[p, 2*num_hints+1]) ||
       (x[P[p,1], P[p,2]] * P[p, 2*num_hints+1] == x[P[p,3], P[p,4]]) ||
       (x[P[p,3], P[p,4]] * P[p, 2*num_hints+1] == x[P[p,1], P[p,2]]) ||
       (x[P[p,1], P[p,2]] - x[P[p,3], P[p,4]] == P[p, 2*num_hints+1]) ||
       (x[P[p,3], P[p,4]] - x[P[p,1], P[p,2]] == P[p, 2*num_hints+1]);
    } else {
        (sum(i in 1..num_hints: P[p,2*(i-1)+1] > 0) x[  P[p, 2*(i-1)+1], P[p,2*(i-1)+2]  ] == P[p, 2*num_hints+1])
        ||
        (prod(i in 1..num_hints: P[p,2*(i-1)+1] > 0) x[  P[p, 2*(i-1)+1], P[p,2*(i-1)+2] ] == P[p, 2*num_hints+1]);
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
