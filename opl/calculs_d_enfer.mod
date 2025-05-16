/* 

  Calculs d'enfer puzzle in OPL.

  Problem from Jianyang Zhou "The Manual of NCL version 1.2", page 33
  http://citeseer.ist.psu.edu/161721.html
  
  The solution is the manual is:
  """
  a = -16, b = -14, c = -13, d = -12, e = -10,
  f = 4, g = 13, h = -1, i = -3, j = -11, k = -9,
  l = 16, m = -8, n = 11, o = 0, p = -6, q = -4,
  r = 15, s = 2, t = 9, u = -15, v = 14, w = -7,
  x = 7, y = -2, z = -5.

  max_{#1\in [1,26]}{|x_{#1}|} minimized to 16
  """

  Also, see the discussion of the Z model:
  http://www.comp.rgu.ac.uk/staff/ha/ZCSP/additional_problems/calculs_enfer/calculs_enfer.ps
  (which shows the same solution).


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int N = 26;
range R = -100..100;

// decision variables
dvar int a in R;
dvar int b in R;
dvar int c in R;
dvar int d in R;
dvar int e in R;
dvar int f in R;
dvar int g in R;
dvar int h in R;
dvar int i in R;
dvar int j in R;
dvar int k in R;
dvar int l in R;
dvar int m in R;
dvar int n in R;
dvar int o in R;
dvar int p in R;
dvar int q in R;
dvar int r in R;
dvar int s in R;
dvar int t in R;
dvar int u in R;
dvar int v in R;
dvar int w in R;
dvar int x in R;
dvar int y in R;
dvar int z in R;

dvar int A[1..N] in R;

// The objective is to minimize the maximum of the absolute values of A[i]
dvar int A_abs[1..N] in R;
dvar int a_max in 0..N;

execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Low"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s = f.searchPhase(A,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );
  cp.setSearchPhases(s);

}

minimize a_max;
constraints {

  A[1] == a;    A[2] == b;    A[3] == c;    A[4] == d;    A[5] == e;
  A[6] == f;    A[7] == g;    A[8] == h;    A[9] == i;    A[10] == j;
  A[11] == k;   A[12] == l;   A[13] == m;   A[14] == n;   A[15] == o;
  A[16] == p;   A[17] == q;   A[18] == r;   A[19] == s;   A[20] == t;
  A[21] == u;   A[22] == v;   A[23] == w;   A[24] == x;   A[25] == y;
  A[26] == z;

  forall(I in 1..N) {   
     A_abs[I] == abs(A[I]);
  }

  forall(I in 1..N) {
    a_max >= A_abs[I];
  }
  
  allDifferent(A);

  z+e+r+o == 0;
  o+n+e == 1;
  t+w+o == 2;
  t+h+r+e+e == 3;
  f+o+u+r == 4;
  f+i+v+e == 5;
  s+i+x == 6;
  s+e+v+e+n == 7;
  e+i+g+h+t == 8;
  n+i+n+e == 9;
  t+e+n == 10;
  e+l+e+v+e+n == 11;
  t+w+e+l+f == 12;

}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      writeln("a_max: ", thisOplModel.a_max);
      writeln("A: ", thisOplModel.A);
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
