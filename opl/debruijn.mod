/* 

  de Bruijn sequences (both "classical" and "arbitrary") in OPL.

  Compare with the the web based programs:
    http://www.hakank.org/comb/debruijn.cgi   
    http://www.hakank.org/comb/debruijn_arb.cgi

  This problem was commented in the (Swedish) blog post
  Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc
  http://www.hakank.org/webblogg/archives/001209.html


  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;

int base =  2;  // the base to use, i.e. the alphabet 0..base-1
int n    =  3;  // number of bits to use (n = 4 -> 0..base^n-1 = 0..2^4 -1, i.e. 0..15)
// int m    = 52;  // the length of the sequence. For "arbitrary" de Bruijn sequences
// int m    = 48;  // the length of the sequence. For "arbitrary" de Bruijn sequences
int m = ftoi(pow(base, n));  // the length of the sequence k^n. Use this for "classic" de Bruijn sequences

int use_gcc = 1; // if we should enforce the gcc constraint (if possible)

// decision variables
dvar int x[1..m] in 0..ftoi(pow(base,n))-1; 
dvar int binary[1..m, 1..n] in 0..base-1; // binary representation of the numbers
dvar int bin_code[1..m] in 0..base-1;     // the sequence in base-representation
dvar int gcc[0..base-1] in 0..m; // number of occurrences of the values in bin_code

execute {
  cp.param.SearchType = "DepthFirst";
  // cp.param.AllDiffInferenceLevel = "Low"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  // cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  var f = cp.factory;
  var s = f.searchPhase(x,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        // f.selectSmallest(f.value())
                        f.selectRandomValue()
                       );
  cp.setSearchPhases(s);


}


constraints {

    // all number must be different
    allDifferent(x);

    // symmetry breaking: the minimum element should be the first element
    // minl(x[1], x);
    forall(i in 1..m) {
       x[1] <= x[i];
    }

    // converts x <-> binary
    forall(i in 1..m) {
       x[i] == sum(j in 1..n) binary[i,j]*ftoi(pow(base,n-j));
    }

    // the de Bruijn condition: 
    // the first elements in binary[i] is the same as the last elements in binary[i-1]
    forall(i in 2..m) {
         forall(j in 2..n) { 
            binary[i-1, j] == binary[i, j-1]; 
         }
    }       
    // ... and around the corner
    forall(j in 2..n) { binary[m, j] == binary[1, j-1]; }

    // converts binary -> bin_code 
    forall(i in 1..m) {
      bin_code[i] == binary[i,1];
    }

    //
    // The constraints below is for the constraint that there should be equal number
    // number of occurrences of "bits".
    // 

    // If m mod base = 0 ->
    // It should be exactly equal number of occurrences
    // otherwise, it should not (and could not).
    // Note: this may take some time for larger problems.

    forall(i in 0..base-1) {
       gcc[i] == count(bin_code, i);
    }

    if (use_gcc == 1 &&  m % base == 0) {
       forall(i in 1..base-1) {
         gcc[i-1] == gcc[i];
         gcc[i] == m / base;
       }
    }

}

main {
   thisOplModel.generate();
   cp.startNewSearch();
   var sol = 0;
   while (cp.next()) {
      writeln("x       :", thisOplModel.x);
      writeln("bin_code:", thisOplModel.bin_code);
      writeln("binary  :", thisOplModel.binary);
      writeln("gcc     :", thisOplModel.gcc);
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
