/* 

  Bales of hay problem in OPL.

  From The Math Less Traveled, 
  "The haybaler", http://www.mathlesstraveled.com/?p=582 
  """
  You have five bales of hay.

  For some reason, instead of being weighed individually, they were weighed 
  in all possible combinations of two. The weights of each of these 
  combinations were written down and arranged in numerical order, without 
  keeping track of which weight matched which pair of bales. The weights, 
  in kilograms, were 80, 82, 83, 84, 85, 86, 87, 88, 90, and 91.

  How much does each bale weigh? Is there a solution? Are there multiple 
  possible solutions? 
  """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 5; 
int b = 10;

int weights[1..b] =  [80, 82, 83, 84, 85, 86, 87, 88, 90, 91];

// decision variables
dvar int bales[1..n] in 0..50;

dvar int itmp[1..b] in 1..n;
dvar int jtmp[1..b] in 1..n;

execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Low"; // "Default", "Low", "Basic", "Medium", "Extended";
  // cp.param.DefaultInferenceLevel="Extended"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s = f.searchPhase(bales,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );
  cp.setSearchPhases(s);

}


constraints {

   forall(w in 1..b) {
       itmp[w] < jtmp[w] &&
       bales[itmp[w]] + bales[jtmp[w]] == weights[w];
   }

   // symmetry breaking
   // increasing(bales);
   forall(i in 2..n) {
     bales[i-1] < bales[i];
   }

}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      var bales = thisOplModel.bales;
      writeln("bales: ", bales);
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
