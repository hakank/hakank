/* 

  Nontransitive dice in OPL.

  From 
  http://en.wikipedia.org/wiki/Nontransitive_dice
  """
  A set of nontransitive dice is a set of dice for which the relation 
  "is more likely to roll a higher number" is not transitive. See also 
  intransitivity.
  
  This situation is similar to that in the game Rock, Paper, Scissors, 
  in which each element has an advantage over one choice and a 
  disadvantage to the other.
  """

  This OPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my OPL page: http://www.hakank.org/opl/

*/

using CP;


int m = ...; // number of dice
int n = ...; // number of sides of each die
int d[1..m, 1..n] = ...; // the data (if any)

// if we want to use the initialization data
int use_data = 0; 


/*
// from the Wikipedia page
//  A > B = 5/9   B > C = 5/9  C > A = 5/9
int m = 3; // number of dice
int n = 6; // number of sides of each die
int wikipedia[1..m, 1..n] = 
[
   [2,2,4,4,9,9], // die A
   [1,1,6,6,8,8], // die B
   [3,3,5,5,7,7]  // die C
];
*/



// decision variables

// the dice
dvar int dice[1..m, 1..n] in 1..n*2;

// the competitions: 
// (die 1 vs die 2, die 2 vs die 1), ... (die m vs die 1, die 1 vs die m)
// And the first die must beat the second in each round.
// Note the last wrap around which breaks the transitivity.
dvar int comp[0..m-1, 1..2] in 0..n*n;

// max value of the dice
dvar int max_val in 0..n*2;
// max win
dvar int max_win in 0..n*n;

// gap, gap_sum
dvar int gap[0..m-1] in 0..n*n;
dvar int gap_sum in 0..n*n*m;



execute {
  cp.param.SearchType = "Restart"; // "DepthFirst", "Restart", "Multipoint";
  // cp.param.AllDiffInferenceLevel = "Extended"; // "Default", "Low", "Basic", "Medium", "Extended";
  cp.param.DefaultInferenceLevel="Extended"; // "Low", "Basic", "Medium", "Extended"
  cp.param.LogVerbosity = "Quiet"; // Quiet, Terse, Normal, Verbose
  cp.param.LogPeriod = 1;
  // Note: If Workers > 1 then we get solutions from each worker
  cp.param.Workers = 1;

  /*
  var f = cp.factory;
  var s = f.searchPhase(dice,
                        // variable
                        f.selectSmallest(f.domainSize()),

                        // value
                        f.selectSmallest(f.value())
                       );

  var s2 = f.searchPhase(comp,
                        // variable
                        f.selectSmallest(f.domainMin()),

                        // value
                        f.selectLargest(f.value())
                       );

  cp.setSearchPhases(s,s2);
  */
 
}

minimize max_val; // minimize the largest value of the dice
// maximize max_win; // maximize the largest winning
// maximize gap_sum; // maximize the sum of the gaps
// maximize sum(i in 0..m-1, j in 1..2) comp[i,j];
// maximize sum(i in 0..m-1) comp[i,1]; // maximize the sum of the winnings
// maximize min(i in 0..m-1) comp[i,1]; // maximize the smallest winning
// maximize min(i in 0..m-1) gap[i]; // maximize the smallest winning

constraints {

    if (use_data == 1) {
      forall(i in 1..m, j in 1..n) {
         dice[i,j] == d[i,j];
      }
    }

    gap_sum == sum(i in 1..m-1) gap[i];
    max_val == max(i in 1..m, j in 1..n) dice[i,j];
    max_win == max(i in 0..m-1, j in 1..2) comp[i,j];

    // order the number of each die (increasing)
    forall(d in 1..m) {
       forall(i in 2..n) {
          dice[d,i-1] <= dice[d,i];
       }   
    }

   // and now we roll...
   // Number of wins for [A vs B, B vs A]
   forall(d in 0..m-1) {
      comp[d,1] == sum(r1, r2 in 1..n) (dice[1+(d mod m), r1] >
                                                dice[1+((d + 1) mod m), r2]);
      comp[d,2] == sum(r1, r2 in 1..n) (dice[1+((d+1) mod m), r1] >
                                                dice[1+((d) mod m), r2]);
      gap[d] == comp[d,1] - comp[d,2];
   }


   // non-transitivity
   // All dice 1..m-1 must beat the follower, and die m must beat die 1
   forall(d in 0..m-1) {
       comp[d,1] > comp[d,2];
   }
 
   // symmetry breaking (be careful if you use one of the examples above)
   /*
   forall(d in 2..m) {
      lex(all(j in 1..n) dice[d-1,j],all(j in 1..n) dice[d,j]);
   } 
   */


}

main {
     thisOplModel.generate();
     cp.startNewSearch();
     var sol = 0;

     while (cp.next()) {
        var t = thisOplModel;
        writeln("dice:  ", t.dice);
        writeln("comp:  ", t.comp);
        writeln("max_win: ", t.max_win);
        writeln("max_val: ", t.max_val);
        writeln("gap    :", t.gap);
        writeln("gap_sum: ", t.gap_sum);
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
