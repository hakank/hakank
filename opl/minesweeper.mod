/* 

  Minesweeper problem in OPL.

  From gecode/examples/minesweeper.cc:
  """
  A specification is a square matrix of characters. Alphanumeric characters represent
  the number of mines adjacent to that field. Dots represent fields with an unknown number
  of mines adjacent to it (or an actual mine).
  """
  
  E.g.
       "..2.3."
       "2....."
       "..24.3"
       "1.34.."
       ".....3"
       ".3.3.."
  """
  
  Also see 
   
  http://www.janko.at/Raetsel/Minesweeper/index.htm
   (the first 10 examples are from)

  http://en.wikipedia.org/wiki/Minesweeper_(computer_game)

  Ian Stewart on Minesweeper: http://www.claymath.org/Popular_Lectures/Minesweeper/

  Richard Kaye's Minesweeper Pages
  http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.htm
  Some Minesweeper Configurations
  http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.pdf


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

/*
int X = ...;
int r = ...;
int c = ...;
int game[1..r, 1..c] = ...;
*/

// Problem from Gecode/examples/minesweeper.cc  problem 0
int X = -1; // the unknowns
int r = 6;
int c = 6;
int game[1..r, 1..c] = 
  [
    [X,X,2,X,3,X],
    [2,X,X,X,X,X],
    [X,X,2,4,X,3],
    [1,X,3,4,X,X],
    [X,X,X,X,X,3],
    [X,3,X,3,X,X]
  ];



// decision variables
dvar int mines[1..r, 1..c] in 0..1; 


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Low"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s = f.searchPhase(mines,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );
  cp.setSearchPhases(s);

}


constraints {

    forall(i in 1..r, j in 1..c: game[i,j] >= 0) {
         game[i,j] == sum(a,b in {-1,0,1}:
                             i+a > 0  && j+b >  0 &&
                             i+a <= r && j+b <= c
                      ) (mines[i+a,j+b]);

      game[i,j] > -1 => mines[i,j] == 0;
    }
 
}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      writeln("mines: ", thisOplModel.mines);
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
