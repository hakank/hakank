/* 

  All interval problem in OPL.

  CSPLib problem number 7
  http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob007/index.html
  """
  Given the twelve standard pitch-classes (c, c%, d, ...), represented by 
  numbers 0,1,...,11, find a series in which each pitch-class occurs exactly 
  once and in which the musical intervals between neighbouring notes cover 
  the full set of intervals from the minor second (1 semitone) to the major 
  seventh (11 semitones). That is, for each of the intervals, there is a 
  pair of neigbhouring pitch-classes in the series, between which this 
  interval appears. The problem of finding such a series can be easily 
  formulated as an instance of a more general arithmetic problem on Z_n, 
  the set of integer residues modulo n. Given n in N, find a vector 
  s = (s_1, ..., s_n), such that (i) s is a permutation of 
  Z_n = {0,1,...,n-1}; and (ii) the interval vector 
  v = (|s_2-s_1|, |s_3-s_2|, ... |s_n-s_{n-1}|) is a permutation of 
  Z_n-{0} = {1,2,...,n-1}. A vector v satisfying these conditions is 
  called an all-interval series of size n; the problem of finding such 
  a series is the all-interval series problem of size n. We may also be 
  interested in finding all possible series of a given size. 
  """


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 9; 
int sum_distinct = ftoi(((n+1)*n) / 2);

// decision variables
dvar int x[1..n] in 1..n;
dvar int diffs[1..n-1] in 1..n-1;


execute {
  cp.param.SearchType = "DepthFirst";
  cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;


  var f = cp.factory;
  var s = f.searchPhase(x,
                        // variable
                        // f.selectSmallest(f.domainSize()),
                        f.selectSmallest(f.impact()),

                        // value
                        f.selectSmallest(f.value())
                       );

  cp.setSearchPhases(s);

}


constraints {

  allDifferent(diffs);
  allDifferent(x);

  forall(k in 1..n-1) {
      diffs[k] == abs(x[k+1] - x[k]);
  }

  // symmetry breaking
  x[1] < x[n-1];

  diffs[1] < diffs[2];

}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      var t = thisOplModel;
      write("x:", t.x, " ", t.sum_distinct, " ");
      writeln("diffs:", t.diffs);
      sol++;

      if (t.n > 12) {
        break;
      }
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
