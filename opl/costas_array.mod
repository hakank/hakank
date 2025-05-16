/* 

  Costas array in OPL.

  Note: This model is based on Barry O'Sullivan's 
  MiniZinc model in the G12 repository:
  http://www.g12.cs.mu.oz.au/mzn/costas_array/CostasArray.mzn


  From http://mathworld.wolfram.com/CostasArray.html:
  """
  An order-n Costas array is a permutation on {1,...,n} such
  that the distances in each row of the triangular difference
  table are distinct. For example, the permutation {1,3,4,2,5}
  has triangular difference table {2,1,-2,3}, {3,-1,1}, {1,2},
  and {4}. Since each row contains no duplications, the permutation
  is therefore a Costas array.
  """

  Also see
  http://en.wikipedia.org/wiki/Costas_array


  About this model (comment from my MiniZinc model 
  http://www.hakank.org/minizinc/costas_array.mzn)
  As mentioned above this model is based on Barry O'Sullivan's 
  model. Here are the two rather simple differences 
  (marked by "hakank" below)
   1) no symmetry breaking on the order of the Costas array
   2) fixes the lower triangular matrix in the difference
      matrix to -n+1
  
  Since there is no symmetry breaking of the order of the Costas 
  array it gives all the solutions for a specific length of 
  the array, e.g. those 
  listed in http://mathworld.wolfram.com/CostasArray.html
  
  1	1	(1)
  2	2	(1, 2), (2,1)
  3	4	(1, 3, 2), (2, 1, 3), (2, 3, 1), (3, 1, 2)
  4	12	(1, 2, 4, 3), (1, 3, 4, 2), (1, 4, 2, 3), (2, 1, 3, 4), 
                (2, 3, 1, 4), (2, 4, 3, 1), (3, 1, 2, 4), (3, 2, 4, 1), 
                (3, 4, 2, 1), (4, 1, 3, 2), (4, 2, 1, 3), (4, 3, 1, 2)
  ....
  
  See http://www.research.att.com/~njas/sequences/A008404
  for the number of solutions for n=1..
  1, 2, 4, 12, 40, 116, 200, 444, 760, 2160, 4368, 7852, 12828, 
  17252, 19612, 21104, 18276, 15096, 10240, 6464, 3536, 2052, 
  872, 200, 88, 56, 204,...
  

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int n = 9;

// decision variables
dvar int costas[1..n] in 1..n;
dvar int differences[1..n, 1..n] in -n+1..n-1;


execute {
  cp.param.SearchType = "DepthFirst";
  cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.DefaultInferenceLevel="Medium"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;


  var f = cp.factory;
  var s = f.searchPhase(differences,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );

  var s2 = f.searchPhase(costas,
                        // variable
                        f.selectSmallest(f.domainMin()),

                        // value
                        f.selectSmallest(f.value())
                       );


  cp.setSearchPhases(s, s2);

 
}



constraints {

      //
      // hakank: Here are my two changes compared to Barry's MiniZinc model
      //
      // 1) I skipped this constraint since I want 
      //    to generate all solutions.
      // constraint costas[1] < costas[n];

      // 2) Fix the values in the lower triangle in the
      // difference matrix to -n+1. This removes variants 
      // of the difference matrix for the the same Costas array.
      forall(i in 1..n) {
          forall(j in 1..i) {
             differences[i,j] == -n+1;
          }
      }

      // hakank: All the following constraints (and comments) are from 
      // Barry O'Sullivans's original model.

      allDifferent(costas);


      // "How do the positions in the Costas array relate 
      //  to the elements of the distance triangle."
      forall(ordered i,j in 1..n) {
	differences[i,j] == costas[j] - costas[j-i];
      }
	

      // "All entries in a particular row of the difference 
      //  triangle must be distint."
      forall(i in 1..n-1) {
	allDifferent(all(j in 1..n: j > i) differences[i,j]);
      }

      // "All the following are redundant - only here to speed up search."

      // "We can never place a 'token' in the same row as any other."
      forall(ordered i,j in 1..n) {
	differences[i,j] != 0;
      }

       forall(k,l in 3..n: k < l) {
	 differences[k-2,l-1] + differences[k,l] == 
	 differences[k-1,l-1] + differences[k-1,l];
       }


}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("costas:", t.costas);
        writeln("differences:");
        for(i = 1; i <= t.n; i++) {
            for(j = 1; j <= t.n; j++) {
              var v = t.differences[i][j];
              if (v > -t.n+1) {
                write(v, " ");
              } else {
                write("  ");
              }
            }
            writeln();
        }
        writeln();
        sol++;
        // don't show all solutions for larger problems
        if (t.n > 9) {
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
