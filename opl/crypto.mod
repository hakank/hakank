/* 

  Cryptarithmetic puzzle in OPL.

  This is the standard benchmark "crypto" problem.

  From GLPK:s model cryto.mod.
  """
  This problem comes from the newsgroup rec.puzzle.
  The numbers from 1 to 26 are assigned to the letters of the alphabet.
  The numbers beside each word are the total of the values assigned to
  the letters in the word (e.g. for LYRE: L, Y, R, E might be to equal
  5, 9, 20 and 13, or any other combination that add up to 47).
  Find the value of each letter under the equations:

  BALLET  45     GLEE  66     POLKA      59     SONG     61
  CELLO   43     JAZZ  58     QUARTET    50     SOPRANO  82
  CONCERT 74     LYRE  47     SAXOPHONE 134     THEME    72
  FLUTE   30     OBOE  53     SCALE      51     VIOLIN  100
  FUGUE   50     OPERA 65     SOLO       37     WALTZ    34

  Solution:
  A, B,C, D, E,F, G, H, I, J, K,L,M, N, O, P,Q, R, S,T,U, V,W, X, Y, Z
  5,13,9,16,20,4,24,21,25,17,23,2,8,12,10,19,7,11,15,3,1,26,6,22,14,18

  Reference:
  Koalog Constraint Solver <http://www.koalog.com/php/jcs.php>,
  Simple problems, the crypto-arithmetic puzzle ALPHACIPHER.
  """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int num_letters = 26;
range LETTERS = 1..num_letters;
int  BALLET     =  45;
int  CELLO      =  43;
int  CONCERT    =  74;
int  FLUTE      =  30;
int  FUGUE      =  50;
int  GLEE       =  66;
int  JAZZ       =  58;
int  LYRE       =  47;
int  OBOE       =  53;
int  OPERA      =  65;
int  POLKA      =  59;
int  QUARTET    =  50;
int  SAXOPHONE  = 134;
int  SCALE      =  51;
int  SOLO       =  37;
int  SONG       =  61;
int  SOPRANO    =  82;
int  THEME      =  72;
int  VIOLIN     = 100;
int  WALTZ      =  34;


// decision variables
dvar int A in LETTERS;
dvar int B in LETTERS;
dvar int C in LETTERS;
dvar int D in LETTERS;
dvar int E in LETTERS;
dvar int F in LETTERS;
dvar int G in LETTERS;
dvar int H in LETTERS;
dvar int I in LETTERS;
dvar int J in LETTERS;
dvar int K in LETTERS;
dvar int L in LETTERS;
dvar int M in LETTERS;
dvar int N in LETTERS;
dvar int O in LETTERS;
dvar int P in LETTERS;
dvar int Q in LETTERS;
dvar int R in LETTERS;
dvar int S in LETTERS;
dvar int T in LETTERS;
dvar int U in LETTERS;
dvar int V in LETTERS;
dvar int W in LETTERS;
dvar int X in LETTERS;
dvar int Y in LETTERS;
dvar int Z in LETTERS;
dvar int all_letters[LETTERS] = [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z];


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  
  var f = cp.factory;
  var s = f.searchPhase(all_letters,
                        // variable
                        f.selectSmallest(f.domainMin()),

                        // value
                        f.selectSmallest(f.value())
                       );
  cp.setSearchPhases(s);

}



constraints {

   allDifferent(all_letters);

   B+A+L+L+E+T       == BALLET;
   C+E+L+L+O         == CELLO;
   C+O+N+C+E+R+T     == CONCERT; 
   F+L+U+T+E         == FLUTE; 
   F+U+G+U+E         == FUGUE; 
   G+L+E+E           == GLEE; 
   J+A+Z+Z           == JAZZ; 
   L+Y+R+E           == LYRE; 
   O+B+O+E           == OBOE; 
   O+P+E+R+A         == OPERA; 
   P+O+L+K+A         == POLKA; 
   Q+U+A+R+T+E+T     == QUARTET; 
   S+A+X+O+P+H+O+N+E == SAXOPHONE; 
   S+C+A+L+E         == SCALE; 
   S+O+L+O           == SOLO; 
   S+O+N+G           == SONG; 
   S+O+P+R+A+N+O     == SOPRANO; 
   T+H+E+M+E         == THEME; 
   V+I+O+L+I+N       == VIOLIN; 
   W+A+L+T+Z         == WALTZ; 

}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("all_letters  :", t.all_letters);
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
