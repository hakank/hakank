/**
 *
 * Magic squares and cards problem in Choco3.
 *
 * Martin Gardner (July 1971)
 * """
 * Allowing duplicates values, what is the largest constant sum for an order-3
 * magic square that can be formed with nine cards from the deck.
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
import solver.search.strategy.IntStrategyFactory;
import solver.search.strategy.selectors.values.InDomainMax;
import solver.search.strategy.selectors.values.InDomainMiddle;
import solver.search.strategy.selectors.values.InDomainMin;
import solver.search.strategy.selectors.values.InDomainRandom;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.explanations.ExplanationFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import solver.search.strategy.selectors.variables.*;
import solver.search.strategy.strategy.Assignment;
import util.ESat;
import util.tools.ArrayUtils;

import java.util.*;

public class MagicSquareAndCards extends AbstractProblem {

  @Option(name = "-n", usage = "Size of grid (default 3).", required = false)
  int n = 3;

  IntVar[][] x;
  IntVar s;
  IntVar[] counts;


  @Override
  public void buildModel() {

    // Max possible value of s: 13+12+..13-n-1
    int max_s = 0;
    for(int i = 13; i > 13-n; i--) {
      max_s += i;
    }
    System.out.println("max_s: " + max_s);

    // A normal deck of cards has 4 of each values 1..13
    x = VariableFactory.enumeratedMatrix("x", n, n, 1, 13, solver);
    s = VariableFactory.bounded("s", 0, max_s, solver);
    counts = VariableFactory.enumeratedArray("counts", 13, 0, 4, solver);

    int[] values = ArrayUtils.oneToN(13);
    solver.post(IntConstraintFactory.global_cardinality(ArrayUtils.flatten(x), values, counts, true));

    // the standard magic square constraints (sans global all_different)
    IntVar[] diag1 = VariableFactory.enumeratedArray("x", n, 1, n*n, solver);
    IntVar[] diag2 = VariableFactory.enumeratedArray("x", n, 1, n*n, solver);
    for(int i = 0; i < n; i++) {

      solver.post(IntConstraintFactory.sum(x[i], s));
      solver.post(IntConstraintFactory.sum(ArrayUtils.getColumn(x, i), s));

      diag1[i] = x[i][i];
      diag2[i] = x[i][n - i - 1];

    }
    solver.post(IntConstraintFactory.sum(diag1, s));
    solver.post(IntConstraintFactory.sum(diag2, s));


    // redundant constraint
    solver.post(IntConstraintFactory.sum(counts,VariableFactory.fixed(n*n, solver)));

  }


  @Override
  public void createSolver() {
    solver = new Solver("MagicSquareAndCards");
  }

  @Override
  public void configureSearch() {
    // solver.set(IntStrategyFactory.firstFail_InDomainMin(ArrayUtils.flatten(x)));
    solver.set(IntStrategyFactory.firstFail_InDomainMax(ArrayUtils.flatten(x))); 
    // solver.set(IntStrategyFactory.firstFail_InDomainMiddle(ArrayUtils.flatten(x)));
    // solver.set(IntStrategyFactory.firstFail_InDomainMiddle(ArrayUtils.append(ArrayUtils.flatten(x), counts)));
    // solver.set(IntStrategyFactory.domOverWDeg_InDomainMin(ArrayUtils.flatten(x), seed));
    // solver.set(new Assignment(new DomOverWDeg(ArrayUtils.flatten(x), seed), new InDomainMax()));
    // solver.set(new Assignment(new MaxRegret(ArrayUtils.flatten(x)), new InDomainMiddle()));

    // These values are from 
    //    choco-parser/src/main/java/parser/flatzinc/ast/FGoal.java
    // solver.set(IntStrategyFactory.ActivityBased(ArrayUtils.flatten(x), solver, 0.999d, 0.2d, 8, 1.1d, 1, seed));

    // Values from 
    //   choco-samples/src/main/java/samples/integer/MagicSquare.java:
    // ImpactBased(IntVar[] VARS, int ALPHA, int SPLIT, int NODEIMPACT, long SEED, boolean INITONLY)
    // * @param VARS       variables of the problem (should be integers)
    // * @param ALPHA      aging parameter
    // * @param SPLIT      split parameter for subdomains computation
    // * @param NODEIMPACT force update of impacts every nodeImpact nodes. Set value to 0 to avoid using it.
    // * @param SEED       a seed for random
    // * @param INITONLY   only apply the initialisation phase, do not update impact thereafter
    // solver.set(new ImpactBased(ArrayUtils.flatten(x), 2, 3, 10, seed, false));
    // solver.set(new ImpactBased(ArrayUtils.flatten(x), 13, n, 0, seed, true));

  }

  @Override
  public void solve() {
    // solver.findSolution();
    solver.findOptimalSolution(ResolutionPolicy.MAXIMIZE, s);
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      do {
        System.out.println("s: " + s.getValue());
        for(int i = 0; i < n; i++) {
          for(int j = 0; j < n; j++) {
            System.out.format("%2d ", x[i][j].getValue());
          }
          System.out.println();
        }
        System.out.println("counts:");
        for(int i = 0; i < 13; i++) {
            System.out.format("%2d ", counts[i].getValue());
        }
        System.out.println();


        num_solutions++;

      } while (solver.nextSolution() == Boolean.TRUE);
      
      System.out.println("It was " + num_solutions + " solutions.");

      
    }  else {
      System.out.println("No solution.");
    }
    
  }


  public static void main(String args[]) {

    new MagicSquareAndCards().execute(args);

  }

}

