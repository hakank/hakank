/**
 *
 * Nontransitive dice in Choco3.
 *
 * From
 * http://en.wikipedia.org/wiki/Nontransitive_dice
 * """
 * A set of nontransitive dice is a set of dice for which the relation
 * 'is more likely to roll a higher number' is not transitive. See also
 * intransitivity.
 *
 * This situation is similar to that in the game Rock, Paper, Scissors,
 * in which each element has an advantage over one choice and a
 * disadvantage to the other.
 * """
 *
 * Choco3 model by Hakan Kjellerstrand (hakank@gmail.com)
 * http://www.hakank.org/choco3/
 *
 */
import org.kohsuke.args4j.Option;
import org.slf4j.LoggerFactory;
import samples.AbstractProblem;
import solver.ResolutionPolicy;
import solver.Solver;
import solver.constraints.Constraint;
import solver.constraints.IntConstraintFactory;
import solver.constraints.IntConstraintFactory.*;
import solver.constraints.nary.cnf.Literal;
import solver.constraints.nary.cnf.Node;
import solver.constraints.nary.cnf.Node.*;
import solver.constraints.nary.cnf.ALogicTree;
import solver.search.strategy.IntStrategyFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import solver.search.strategy.selectors.variables.*;
import solver.search.strategy.strategy.Assignment;

import util.ESat;
import util.tools.ArrayUtils;

public class NontransitiveDice extends AbstractProblem {

  @Option(name = "-n", usage = "n (sides per die, default 6).", required = false)
  int n = 6;

  @Option(name = "-m", usage = "m (number of dice, default 3).", required = false)
  int m = 3;

  @Option(name = "-max", usage = "max (max value of dice, default n*2).", required = false)
  int max = 0;

  // Conjecture: 4 is the minimum max value for any nontransitive dice?
  @Option(name = "-min_max", usage = "min (minimum max value of dice, default 1).", required = false)
  int min_max = 1;


  @Option(name = "-solutions", usage = "number of solutions (default 1).", required = false)
  int solutions = 1;

  @Option(name = "-minimize_max_val", usage = "minimize max_val (default false).", required = false)
  boolean minimize_max_val = false;

  @Option(name = "-maximize_max_val", usage = "maximize max_val (default false).", required = false)
  boolean maximize_max_val = false;

  @Option(name = "-minimize_max_win", usage = "minimize max_win (default false).", required = false)
  boolean minimize_max_win = false;

  @Option(name = "-maximize_max_win", usage = "maximize max_win (default false).", required = false)
  boolean maximize_max_win = false;

  @Option(name = "-minimize_gap_sum", usage = "minimize max_sum (default false).", required = false)
  boolean minimize_gap_sum = false;

  @Option(name = "-maximize_gap_sum", usage = "maximize max_sum (default false).", required = false)
  boolean maximize_gap_sum = false;

  // Experimental
  @Option(name = "-lex", usage = "use lex_chain (default false).", required = false)
  boolean lex = false;




  // The dice
  IntVar[][] dice;
  IntVar[] dice_flat;

  // For comparison (probability)
  IntVar[][] comp;
  IntVar[] comp_flat;

  // These are for summaries of objectives
  IntVar[] gap;
  IntVar gap_sum;
  IntVar max_val;
  IntVar max_win;

  // Number of occurrences of each value of the dice
  IntVar[] counts;


  @Override
  public void buildModel() {

    if (max == 0) {
      max = n*2;
    }

    

    // Decision variables
    dice = VariableFactory.enumeratedMatrix("dice", m, n, 1, max, solver);
    dice_flat = ArrayUtils.flatten(dice);

    comp = VariableFactory.boundedMatrix("comp", m, 2, 0, n*n, solver);
    comp_flat = ArrayUtils.flatten(comp);

    counts = VariableFactory.enumeratedArray("counts", max+1, 0, m*n*n, solver);

    gap = VariableFactory.boundedArray("gap", m, 0, n*n, solver);

    max_val = VariableFactory.bounded("max_val", min_max, max, solver);
    max_win = VariableFactory.bounded("max_win", 0, n*m, solver);
    gap_sum = VariableFactory.bounded("gap_sum", 0, n*n, solver);


    // Constraints
    solver.post(IntConstraintFactory.maximum(max_val, dice_flat));
    solver.post(IntConstraintFactory.maximum(max_win, comp_flat));

    // Number of occurrences for each number
    int[] values = ArrayUtils.zeroToN(max+1);
    solver.post(IntConstraintFactory.global_cardinality(dice_flat, values, counts, true));

    // Experimental: use lex_change_less
    if (lex) {
      solver.post(IntConstraintFactory.lex_chain_less_eq(dice));
    }

    // Order of the number of each die, lowest first
    for(int i = 0; i < m; i++) {
      for(int j = 0; j < n-1; j++) {
        solver.post(IntConstraintFactory.arithm(dice[i][j], "<=", dice[i][j+1]));
      }
    }

    // Nontransitivity
    for(int i = 0; i < m; i++) {
      solver.post(IntConstraintFactory.arithm(comp[i][0],">", comp[i][1]));
    }

    // Probability gap
    for(int i = 0; i < m; i++) {
      // gap[i] == comp[i,0] - comp[i,1]
      solver.post(IntConstraintFactory.sum(new IntVar[] {comp[i][0], VariableFactory.minus(comp[i][1])},gap[i] ));
      solver.post(IntConstraintFactory.arithm(gap[i],">", VariableFactory.fixed(0, solver)));
    }
    solver.post(IntConstraintFactory.sum(gap, gap_sum));

    // And now we roll...
    // comp[] is the number of wins for [A vs B, B vs A]
    for(int d = 0; d < m; d++) {
      BoolVar[][] sum1B = VariableFactory.boolMatrix("sum1B_"+d, n, n,solver);
      for(int r1 = 0; r1 < n; r1++) {
        for(int r2 = 0; r2 < n; r2++) {
          solver.post(IntConstraintFactory.implies(sum1B[r1][r2],
                                                   IntConstraintFactory.arithm(dice[d % m][r1],">", dice[(d+1)%m][r2])));
          solver.post(IntConstraintFactory.implies(VariableFactory.not(sum1B[r1][r2]),
                                                   IntConstraintFactory.arithm(dice[d % m][r1],"<=", dice[(d+1)%m][r2])));

        }
      }
      solver.post(IntConstraintFactory.sum(ArrayUtils.flatten(sum1B), comp[d%m][0]));

      BoolVar[][] sum2B = VariableFactory.boolMatrix("sum2B_"+d, n, n,solver);
      for(int r1 = 0; r1 < n; r1++) {
        for(int r2 = 0; r2 < n; r2++) {
          solver.post(IntConstraintFactory.implies(sum2B[r1][r2],
                                                   IntConstraintFactory.arithm(dice[(d+1) % m][r1],">", dice[d%m][r2])));
          solver.post(IntConstraintFactory.implies(VariableFactory.not(sum2B[r1][r2]),
                                                   IntConstraintFactory.arithm(dice[(d+1) % m][r1],"<=", dice[d%m][r2])));
        }
      }
      solver.post(IntConstraintFactory.sum(ArrayUtils.flatten(sum2B), comp[d%m][1]));
    }

  }

  @Override
  public void createSolver() {
    solver = new Solver("NontransitiveDice");
  }

  @Override
  public void configureSearch() {
    // solver.set(IntStrategyFactory.domOverWDeg_InDomainMin(ArrayUtils.append(dice_flat, counts), seed));
    // solver.set(IntStrategyFactory.domOverWDeg_InDomainMin(dice_flat, seed));
    // solver.set(IntStrategyFactory.firstFail_InDomainMin(dice_flat));
    solver.set(IntStrategyFactory.ActivityBased(dice_flat, solver, 0.999d, 0.999d, 2, 1.1d, 0, seed)); 
    // solver.set(new ImpactBased(dice_flat, 2, 3, 10, seed, false));
  }

  @Override
  public void solve() {
    if (minimize_max_val) {

      solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, max_val);

    } else if (maximize_max_val) {

      solver.findOptimalSolution(ResolutionPolicy.MAXIMIZE, max_val);

    } else if (minimize_max_win) {

      solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, max_win);

    } else if (maximize_max_win) {

      solver.findOptimalSolution(ResolutionPolicy.MAXIMIZE, max_win);

    } else if (minimize_gap_sum) {

      solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, gap_sum);

    } else if (maximize_gap_sum) {

      solver.findOptimalSolution(ResolutionPolicy.MAXIMIZE, gap_sum);

    } else {

      solver.findSolution();

    }
  }


  @Override
  public void prettyOut() {
 
    if (solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      do {
        System.out.println("gap_sum: " + gap_sum.getValue());
        System.out.println("max_val: " + max_val.getValue());
        System.out.println("max_win: " + max_win.getValue());
        System.out.println("dice:");
        for(int i = 0; i < m; i++) {
          for(int j = 0; j < n; j++) {
            System.out.format("%2d ", dice[i][j].getValue());
          }
          System.out.println();
        }
        System.out.println();
        System.out.println("comp:");
        for(int i = 0; i < m; i++) {
          for(int j = 0; j < 2; j++) {
            System.out.print(comp[i][j].getValue() + " ");
          }
          System.out.println();
        }
        System.out.println();
        System.out.println("counts:");
        for(int i = 1; i < max+1; i++) {
          int c = counts[i].getValue();
          // if (c > 0) {
            System.out.format("%d(%d) ", i, c);
          //}
        }
        System.out.println();


        num_solutions++;
        
        if (solutions > 0 && num_solutions >= solutions) {
          break;
        }

      } while (solver.nextSolution() == Boolean.TRUE);


      System.out.println("\nIt was " + num_solutions + " solutions.");

    }  else {
      System.out.println("No solution.");
    }

  }


  public static void main(String args[]) {

    new NontransitiveDice().execute(args);

  }


}

