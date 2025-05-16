/* 

  Cyclohexane problem  in OPL.
  (Circling the Squares puzzle)

  From the Oz examples
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/circlingsquares.html
  """
  From "Amusements in Mathematics, Dudeney",
  number 43.

  The puzzle is to place a different number in each of the ten squares
  so that the sum of the squares of any two adjacent numbers shall be
  equal to the sum of the squares of the two numbers diametrically
  opposite to them. The four numbers placed, as examples, must stand as
  they are. Fractions are not allowed, and no number need contain more
  than two figures.
  """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

range r = 1..99; 

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
// dvar int J in r; // not used
dvar int K in r;

dvar int x[1..10] = [A,B,C,D,E,F,G,H,I,K];

execute {
  cp.param.SearchType = "DepthFirst";
  cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.DefaultInferenceLevel="Extended"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;


  var f = cp.factory;
  var s = f.searchPhase(x,
                        // variable
                        f.selectSmallest(f.domainMin()),

                        // value
                        f.selectSmallest(f.value())
                       );

  cp.setSearchPhases(s);

 
}

constraints {

    allDifferent(x);
    A == 16;
    B == 2;
    F == 8;
    G == 14;

    A*A + B*B == F*F + G*G;
    B*B + C*C == G*G + H*H;
    C*C + D*D == H*H + I*I;
    D*D + E*E == I*I + K*K;
    E*E + F*F == K*K + A*A;


}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("x:", t.x);
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
