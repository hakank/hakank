/* 

  Cross word puzzle in OPL.

  This is a standard example for constraint logic programming. See e.g.
  http://www.cis.temple.edu/~ingargio/cis587/readings/constraints.html
  """
  We are to complete the puzzle

       1   2   3   4   5
     +---+---+---+---+---+       Given the list of words:
   1 | 1 |   | 2 |   | 3 |             AFT     LASER
     +---+---+---+---+---+             ALE     LEE
   2 | # | # |   | # |   |             EEL     LINE
     +---+---+---+---+---+             HEEL    SAILS
   3 | # | 4 |   | 5 |   |             HIKE    SHEET
     +---+---+---+---+---+             HOSES   STEER
   4 | 6 | # | 7 |   |   |             KEEL    TIE
     +---+---+---+---+---+             KNOT
   5 | 8 |   |   |   |   |
     +---+---+---+---+---+       
   6 |   | # | # |   | # |       The numbers 1,2,3,4,5,6,7,8 in the crossword
     +---+---+---+---+---+       puzzle correspond to the words 
                                                   that will start at those locations.
  """

  The model was inspired by Sebastian Brand's Array Constraint cross word example
  http://www.cs.mu.oz.au/~sbrand/project/ac/
  http://www.cs.mu.oz.au/~sbrand/project/ac/examples.pl

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int a =  1; int b =  2; int c =  3; int d =  4; int e =  5; int f =  6;
int g =  7; int h =  8; int i =  9; int j = 10; int k = 11; int l = 12;
int m = 13; int n = 14; int o = 15; int p = 16; int q = 17; int r = 18;
int s = 19; int t = 20; int u = 21; int v = 22; int w = 23; int x = 24;
int y = 25; int z = 26; 

int N = 8;

//
// The overlapping positions in the crossword, i.e.
// where the letters is the same
//
int num_overlapping = 12;
int overlapping[1..num_overlapping, 1..4] = 
[
 [1, 3, 2, 1], 
 [1, 5, 3, 1], 
 
 [4, 2, 2, 3], 
 [4, 3, 5, 1], 
 [4, 4, 3, 3], 
 
 [7, 1, 2, 4], 
 [7, 2, 5, 2], 
 [7, 3, 3, 4], 
 
 [8, 1, 6, 2], 
 [8, 3, 2, 5], 
 [8, 4, 5, 3], 
 [8, 5, 3, 5] 
 ];

int num_words = 15;
// Definition of the words. A zero is used to fill the row.
int A[1..num_words,1..5] = 
[
  [h, o, s, e, s],     //  HOSES
  [l, a, s, e, r],     //  LASER
  [s, a, i, l, s],     //  SAILS
  [s, h, e, e, t],     //  SHEET
  [s, t, e, e, r],     //  STEER
  [h, e, e, l, 0],     //  HEEL
  [h, i, k, e, 0],     //  HIKE
  [k, e, e, l, 0],     //  KEEL
  [k, n, o, t, 0],     //  KNOT
  [l, i, n, e, 0],     //  LINE
  [a, f, t, 0, 0],     //  AFT
  [a, l, e, 0, 0],     //  ALE
  [e, e, l, 0, 0],     //  EEL
  [l, e, e, 0, 0],     //  LEE
  [t, i, e, 0, 0]      //  TIE
];

string alpha[0..26] = [" ", "a","b","c","d","e","f","g","h","i","j","k","l","m",
                       "n","o","p","q","r","s","t","u","v","w","x","y","z"];


// decision variables
dvar int E[1..N] in 1..num_words;


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Low"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s = f.searchPhase(E,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );
  cp.setSearchPhases(s);


}


constraints {
  // check all overlapping positions
  forall(i in 1..num_overlapping) {
    A[E[overlapping[i,1]], overlapping[i,2]] ==  A[E[overlapping[i,3]], overlapping[i,4]];
  }
  // redundant constraint
  allDifferent(E);

}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      var alpha = thisOplModel.alpha;   
      var N = thisOplModel.N;
      var E = thisOplModel.E;
      var A = thisOplModel.A;
      writeln("E: ", thisOplModel.E);
      for(i = 1; i <= N; i++) {
        write(i, ": ");
        if (E[i] < 10) {
          write(" ");
        }
        write(E[i], "  ");
        for(j = 1; j <= 5; j++) {
          write(alpha[A[E[i]][j]], " ");
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
