/* 

  Survo puzzle  in OPL.

  http://en.wikipedia.org/wiki/Survo_Puzzle
  """
  Survo puzzle is a kind of logic puzzle presented (in April 2006) and studied 
  by Seppo Mustonen. The name of the puzzle is associated to Mustonen's 
  Survo system which is a general environment for statistical computing and 
  related areas.
  
  In a Survo puzzle the task is to fill an m * n table by integers 1,2,...,m*n so 
  that each of these numbers appears only once and their row and column sums are 
  equal to integers given on the bottom and the right side of the table. 
  Often some of the integers are given readily in the table in order to 
  guarantee uniqueness of the solution and/or for making the task easier.
  """
  
  See also
  http://www.survo.fi/english/index.html
  http://www.survo.fi/puzzles/index.html

  References:
  Mustonen, S. (2006b). "On certain cross sum puzzles"
  http://www.survo.fi/papers/puzzles.pdf 
  Mustonen, S. (2007b). "Enumeration of uniquely solvable open Survo puzzles." 
  http://www.survo.fi/papers/enum_survo_puzzles.pdf 
  Kimmo Vehkalahti: "Some comments on magic squares and Survo puzzles" 
  http://www.helsinki.fi/~kvehkala/Kimmo_Vehkalahti_Windsor.pdf
  R code: http://koti.mbnet.fi/tuimala/tiedostot/survo.R


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

// Problem from 
// http://en.wikipedia.org/wiki/Survo_Puzzle, first example
int r = 3;
int c = 4;
int rowsums[1..r] = [30,18,30];
int colsums[1..c] = [27,16,10,25];
int matrix[1..r, 1..c] = 
   [
     [0, 6, 0, 0],
     [8, 0, 0, 0],
     [0, 0, 3, 0] 
   ];


// decision variables
dvar int x[1..r, 1..c] in 1..r*c;


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

  allDifferent(all(i in 1..r, j in 1..c) x[i,j]);


  forall(i in 1..r, j in 1..c: matrix[i,j] > 0) {
     matrix[i,j] == x[i,j];
  }

  forall(i in 1..r) {
     sum(j in 1..c) (x[i,j]) == rowsums[i];
  }

  forall(j in 1..c) {
    sum(i in 1..r) (x[i,j]) == colsums[j];
  }

}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      var x = thisOplModel.x;
      var r = thisOplModel.r;
      var c = thisOplModel.c;
      var rowsums = thisOplModel.rowsums;
      var colsums = thisOplModel.colsums;
      writeln();
      for(var i = 1; i <= r; i++) {
        write("  ");
        for(var j = 1; j <= c; j++) {
           var v = x[i][j];
           if (v < 10) {
             write(" ");
           }
           write(v, " ");
        }
        writeln(" = ", rowsums[i]);
      }
      writeln(colsums);
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
