/* 

  Labeled dice in OPL.

  From Jim Orlin "Colored letters, labeled dice: a logic puzzle"
  http://jimorlin.wordpress.com/2009/02/17/colored-letters-labeled-dice-a-logic-puzzle/
  """
  My daughter Jenn bough a puzzle book, and showed me a cute puzzle.  There 
  are 13 words as follows:  BUOY, CAVE, CELT, FLUB, FORK, HEMP, JUDY, 
  JUNK, LIMN, QUIP, SWAG, VISA, WISH.

  There are 24 different letters that appear in the 13 words.  The question 
  is:  can one assign the 24 letters to 4 different cubes so that the 
  four letters of each word appears on different cubes.  (There is one 
  letter from each word on each cube.)  It might be fun for you to try 
  it.  I'll give a small hint at the end of this post. The puzzle was 
  created by Humphrey Dudley.
  """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 4;
int use_symmetry_breaking = 1;

range Cube = 1..n;
int num_words = 13;
int A = 1;
int B = 2;
int C = 3;
int D = 4;
int E = 5;
int F = 6;
int G = 7;
int H = 8;
int I = 9;
int J = 10;
int K = 11;
int L = 12;
int M = 13;
int N = 14;
int O = 15;
int P = 16;
int Q = 17;
int R = 18;
int S = 19;
int T = 20;
int U = 21;
int V = 22;
int W = 23;
int Y = 24;

// The words
int words[1..num_words, 1..n] = 
  [
   [B,U,O,Y],
   [C,A,V,E], 
   [C,E,L,T], 
   [F,L,U,B], 
   [F,O,R,K], 
   [H,E,M,P], 
   [J,U,D,Y], 
   [J,U,N,K], 
   [L,I,M,N], 
   [Q,U,I,P], 
   [S,W,A,G], 
   [V,I,S,A], 
   [W,I,S,H]
  ];


// decision variables
dvar int dice[1..24] in Cube;


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Low"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s = f.searchPhase(dice,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );

  cp.setSearchPhases(s);

}


constraints {
  // The letters in a word must be on a different die
  forall(i in 1..num_words) {
    allDifferent(all(j in 1..n) dice[words[i,j]]);
  }

  // There must be exactly 6 letters of each die
  forall(i in 1..n) {
    count(dice, i) == 6;
  }

  // symmetry breaking
  if (use_symmetry_breaking == 1) {
     dice[ 1] <= dice[ 7];
     dice[ 7] <= dice[13];
     dice[13] <= dice[19];
  }

}
/*
Die 1: a h j l o q 
Die 2: c f n p s y 
Die 3: b d e g i k 
Die 4: m r t u v w 
*/
main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;
     var alpha = " abcdefghijklmnopqrstuvwy";
     while (cp.next()) {
        var t = thisOplModel;
        writeln("dice:", t.dice);
        for(d = 1; d <= t.n; d++) {
            write("Die", d, ": ");
            for(j = 1; j <= 24; j++) {
              if (t.dice[j] == d) {
                write(alpha.charAt(j), " ");
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
