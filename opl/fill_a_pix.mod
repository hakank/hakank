/* 

  Fill-a-Pix puzzle in OPL.

  From http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/basiclogic
  """
  Each puzzle consists of a grid containing clues in various places. The 
  object is to reveal a hidden picture by painting the squares around each 
  clue so that the number of painted squares, including the square with 
  the clue, matches the value of the clue. 
  """

  Other names of this puzzle:

      * ぬり絵パズル
      * Nurie-Puzzle
      * Majipiku
      * Oekaki-Pix
      * Mosaic
      * Mosaik
      * Mozaïek
      * ArtMosaico
      * Count and Darken
      * Nampre puzzle
      * Komsu Karala!
      * Cuenta Y Sombrea
      * Mosaico
      * Voisimage
      * Magipic
      * Fill-In

  http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
  """
  Fill-a-Pix is a Minesweeper-like puzzle based on a grid with a pixilated 
  picture hidden inside. Using logic alone, the solver determines which 
  squares are painted and which should remain empty until the hidden picture 
  is completely exposed.
  """
  
  Fill-a-pix History:
  http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/history

  Also, compare with the Minesweeper model
     http://www.hakank.org/opl/minesweeper.mod

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int X = -1; // blank

/*
// Puzzle 1 from 
// http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
// 
int n = 10;
int puzzle[1..n, 1..n] = 
[
  [X,X,X,X,X,X,X,X,0,X],
  [X,8,8,X,2,X,0,X,X,X],
  [5,X,8,X,X,X,X,X,X,X],
  [X,X,X,X,X,2,X,X,X,2],
  [1,X,X,X,4,5,6,X,X,X],
  [X,0,X,X,X,7,9,X,X,6],
  [X,X,X,6,X,X,9,X,X,6],
  [X,X,6,6,8,7,8,7,X,5],
  [X,4,X,6,6,6,X,6,X,4],
  [X,X,X,X,X,X,3,X,X,X]
];
*/

/*
// Puzzle 2 from 
// http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
// 
int n = 10;
int puzzle[1..n, 1..n] = 
[
  [0,X,X,X,X,X,3,4,X,3],
  [X,X,X,4,X,X,X,7,X,X],
  [X,X,5,X,2,2,X,4,X,3],
  [4,X,6,6,X,2,X,X,X,X],
  [X,X,X,X,3,3,X,X,3,X],
  [X,X,8,X,X,4,X,X,X,X],
  [X,9,X,7,X,X,X,X,5,X],
  [X,X,X,7,5,X,X,3,3,0],
  [X,X,X,X,X,X,X,X,X,X],
  [4,4,X,X,2,3,3,4,3,X]
];
*/


// Puzzle from 
// http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/basiclogic
//
// Code: 030.15x15
// ID: 03090000000
// 
int n = 15;
int puzzle[1..n, 1..n] = 
[
  [X,5,X,6,X,X,X,X,X,X,6,X,X,X,X],
  [X,X,7,6,X,4,X,X,4,X,X,8,9,X,5],
  [5,X,X,5,X,5,X,3,X,6,X,7,X,X,6],
  [4,X,2,X,4,X,4,X,3,X,2,X,X,9,X],
  [X,X,X,5,X,4,X,3,X,4,X,4,5,X,6],
  [X,4,3,3,4,X,X,X,4,X,2,X,X,X,X],
  [X,X,X,X,X,X,X,X,X,5,X,X,X,4,X],
  [3,X,3,X,X,3,X,X,X,5,X,4,4,X,X],
  [X,X,X,4,3,X,3,3,X,X,5,7,6,X,X],
  [4,X,X,X,2,X,3,3,2,X,8,9,X,5,X],
  [X,X,3,X,X,X,X,5,X,X,7,X,8,X,X],
  [4,X,X,3,2,X,X,X,X,X,7,X,X,6,X],
  [X,X,4,X,5,4,4,X,X,9,6,X,X,X,X],
  [X,3,5,7,X,6,X,X,X,X,X,X,7,X,X],
  [X,X,4,6,6,X,X,X,6,5,X,X,X,4,X]
];



// decision variables
dvar int x[1..n, 1..n] in 0..1;


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

    // Cf // http://www.hakank.org/opl/minesweeper.mod
    forall(i,j in 1..n: puzzle[i,j] >= 0) {
        puzzle[i,j] == sum(a,b in {-1,0,1}:
                           i+a > 0  && j+b >  0 &&
                           i+a <= n && j+b <= n
                          )
                          x[i+a,j+b];
    }

}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        //writeln("x:", t.x);
        for(i = 1; i <= t.n; i++) {
          for(j = 1; j <= t.n; j++) {
            var v = t.x[i][j];
            if (v == 1) {
              write("#");
            } else {
              write(" ");
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
