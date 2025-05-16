/* 

  Fractions problem in OPL.

  Prolog benchmark problem (BProlog)
  """
  Find distinct non-zero digits such that the following equation holds:
         A        D        G
      ------  + ----- + ------  = 1
        B*C      E*F      H*I
  """


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 9;
range r = 1..9;

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

dvar int x[1..9] = [A,B,C,D,E,F,G,H,I];
dvar int D1 in 1..81;
dvar int D2 in 1..81;
dvar int D3 in 1..81;


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.DefaultInferenceLevel="Extended"; // "Low", "Basic", "Medium", "Extended"
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

   D1 == B*C;
   D2 == E*F;
   D3 == H*I;
   A*D2*D3 + D*D1*D3 + G*D1*D2 == D1*D2*D3;

   // break the symmetry
   A*D2 >= D*D1;
   D*D3 >= G*D2;

   //redundant constraints
   3*A >= D1;
   3*G <= D2;

}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("x:", t.x);
        writeln("D1:", t.D1);
        writeln("D2:", t.D2);
        writeln("D3:", t.D3);
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
