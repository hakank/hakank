/* 

  Twin letters problem in OPL.

  From
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/index.html
  """
  Twin Letters    

  In the following puzzle, there are ten pairs of
  letters to be assigned to the same digit so that the multiplication
  (including intermediate results) is correct. Can you find out the
  pairs and their values?

          A B C
   *      D E F
   ____________
          G H I
        J K L
      M N O
   ____________
      P Q R S T
  """


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 20;
range r = 0..9;


// decision variables

dvar int A in r;
dvar int B in r;
dvar int C in r;
dvar int D in r;
dvar int E in r;
dvar int F in r;
dvar int G in r;
dvar int H in r;
dvar int I in r;
dvar int J in r;
dvar int K in r;
dvar int L in r;
dvar int M in r;
dvar int N in r;
dvar int O in r;
dvar int P in r;
dvar int Q in r;
dvar int R in r;
dvar int S in r;
dvar int T in r;

dvar int x[1..n] = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T];

dvar int C1 in 0..1;
dvar int C2 in 0..2;
dvar int C3 in 0..1;


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Medium"; // "Default", "Low", "Basic", "Medium", "Extended";

  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s = f.searchPhase(x);
  cp.setSearchPhases(s);

  /*
  var f = cp.factory;
  var s = f.searchPhase(x,
                        // variable
                        f.selectSmallest(f.domainMin()),
                        // value
                        f.selectSmallest(f.value())
                       );

  cp.setSearchPhases(s);
  */


}

constraints {

   // exact 2 occurrences of each digit
   forall(i in 0..9) {
     count(x, i) == 2;
   }

            100*G + 10*H + I +
   1000*J + 100*K + 10*L +
   10000*M + 1000*N + 100*O ==
   10000*P + 1000*Q + 100*R + 10*S + T;
    
   (100*D + 10*E + F)*C == 100*G + 10*H + I;
   (100*D + 10*E + F)*B == 100*J + 10*K + L;
   (100*D + 10*E + F)*A == 100*M + 10*N + O;
    
   (100*A + 10*B + C) * (100*D + 10*E + F) ==
   10000*P + 1000*Q + 100*R + 10*S + T;


   // carry restrictions
   T         == I;
   S + 10*C1 == H + L;
   R + 10*C2 == G + K + O + C1;
   Q + 10*C3 == J + N + C2;
   P         == M + C3;
;




}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      var t = thisOplModel;
      writeln("x: ", t.x);
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
