/* 

  General store problem in OPL.

  From
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/index.html
  """
  The General Store  from "Mathematical Puzzles of Sam Loyd", number 30

  The owner of a general store, who is something of a puzzlist, has put
  up this sign to see if any of his mathematical friends can translate
  it properly. Each different letter stands for a different digit. The
  words above the horizontal line represent numbers that add to the
  total of "ALL WOOL". The problem is to change all the letters to the
  correct digits.

         C H E S S
   +       C A S H
   +   B O W W O W
   +     C H O P S
   +   A L S O P S
   + P A L E A L E
   +       C O O L
   +       B A S S
   +       H O P S
   +       A L E S
   +       H O E S
   +   A P P L E S
   +       C O W S 
   +   C H E E S E
   +   C H S O A P
   +     S H E E P
   _______________
     A L L W O O L
   """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 10;
range r = 0..9;

// decision variables
dvar int C in r;
dvar int H in r;
dvar int E in r;
dvar int S in r;
dvar int A in r;
dvar int B in r;
dvar int O in r;
dvar int W in r;
dvar int P in r;
dvar int L in r;

dvar int x[1..n] = [C,H,E,S,A,B,O,W,P,L];

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

                            10000*C + 1000*H + 100*E + 10*S + S
   +                                  1000*C + 100*A + 10*S + H
   +             100000*B + 10000*O + 1000*W + 100*W + 10*O + W
   +                        10000*C + 1000*H + 100*O + 10*P + S
   +             100000*A + 10000*L + 1000*S + 100*O + 10*P + S
   + 1000000*P + 100000*A + 10000*L + 1000*E + 100*A + 10*L + E
   +                                  1000*C + 100*O + 10*O + L
   +                                  1000*B + 100*A + 10*S + S
   +                                  1000*H + 100*O + 10*P + S
   +                                  1000*A + 100*L + 10*E + S
   +                                  1000*H + 100*O + 10*E + S
   +             100000*A + 10000*P + 1000*P + 100*L + 10*E + S
   +                                  1000*C + 100*O + 10*W + S
   +             100000*C + 10000*H + 1000*E + 100*E + 10*S + E
   +             100000*C + 10000*H + 1000*S + 100*O + 10*A + P
   +                        10000*S + 1000*H + 100*E + 10*E + P   
  == 1000000*A + 100000*L + 10000*L + 1000*W + 100*O + 10*O + L;


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
