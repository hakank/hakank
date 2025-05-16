/* 

  Added corner puzzle in OPL.

  Problem from http://www.delphiforfun.org/Programs/AddedCorners.htm
  """
  This puzzle requires that you enter the digits 1 through 8 in the 
  circles and squares (one digit in each figure) so that the number in 
  each square is equal to the sum on the numbers in the circles which
  adjoin it.  
  ...
  
     C F C
     F   F
     C F C

  """


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 8;
range r = 1..n;


// decision variables
dvar int A in r;
dvar int B in r;
dvar int C in r;
dvar int D in r;
dvar int E in r;
dvar int F in r;
dvar int G in r;
dvar int H in r;

dvar int x[1..n] = [A,B,C,D,E,F,G,H];


execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
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
    B == A + C;
    D == A + F;
    E == C + H;
    G == F + H;


}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("x:", t.x);
        writeln(t.A, " ", t.B, " ", t.C);
        writeln(t.D, "   ", t.E);
        writeln(t.F, " ", t.G, " ", t.H);
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
