/* 

  Langford's number problem in OPL.

  This model is based on the ESSENCE' model in the Minion Translator examples:
  http://www.cs.st-andrews.ac.uk/~andrea/examples/langford/langford.eprime
  """
  Langford's number problem (CSP lib problem 24)

  Arrange 2 sets of positive integers 1..k to a sequence,
  such that, following the first occurence of an integer i, 
  each subsequent occurrence of i, appears i+1 indices later
  than the last. 
  For example, for k=4, a solution would be 41312432
  """
  
  Also see: http://www.csplib.org/prob/prob024/

  However, I added a better representation were we see the numbers in their
  proper positions: the solution array.


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int k = 11;

range dom = 1..2*k;

// decision variables
dvar int position[dom] in dom;
dvar int solution[dom] in 1..k;

execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Low"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s = f.searchPhase(position,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );

  cp.setSearchPhases(s);


}


constraints {

  allDifferent(position);

  forall(i in 1..k) {
     position[i+k] == position[i] + i+1;
     solution[position[i]] == i;
     solution[position[k+i]] == i;
  }


  // symmetry breaking
  solution[1] > solution[2*k];



}

main {
   var k = thisOplModel.k;
   if (k % 4 != 0 && k % 4 != 3) {

      writeln("Error: k must be either 0 mod 4 or 3 mod 4");

   } else {

     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;
     while (cp.next()) {
        writeln("position:", thisOplModel.position);
        writeln("solution:", thisOplModel.solution);
        writeln();
        if (k > 11) {
          break;
        }
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

} 
