/**
 *
 * Coin problem in Choco3.
 *
 * From "Constraint Logic Programming using ECLiPSe"
 *  pages 99f and 234 ff.
 * The solution in ECLiPSe is at page 236.
 *
 * """
 * What is the minimum number of coins that allows one to pay _exactly_
 * any amount smaller than one Euro? Recall that there are six different
 * euro cents, of denomination 1, 2, 5, 10, 20, 50
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
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.explanations.ExplanationFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import solver.search.strategy.selectors.variables.*;
import util.ESat;
import util.tools.ArrayUtils;

import java.util.*;

public class Coins3 extends AbstractProblem {

  @Option(name = "-show_all_opt", usage = "Show all optimal solutions (default false).", required = false)
  boolean show_all_opt = false;

  @Option(name = "-opt_val", usage = "The optimal value (default 8).", required = false)
  int opt_val = 8;


  int n = 6; // number of different coins
  // the coin denominations
  int[] variables = {1, 2, 5, 10, 25, 50};


  IntVar[] x;
  IntVar num_coins;


  @Override
  public void buildModel() {
  
    x = VariableFactory.enumeratedArray("x", n, 0, 99, solver);
    num_coins = VariableFactory.bounded("num_coins", 0, 99, solver);

    solver.post(IntConstraintFactory.sum(x, num_coins));

    
    // Check that all changes from 1 to 99 can be made.
    for(int j = 1; j < 100; j++) {
      IntVar[] tmp = VariableFactory.boundedArray("tmp_"+j, n, 0, 99, solver);
      solver.post(IntConstraintFactory.scalar(tmp, variables, VariableFactory.fixed(j, solver)));      
      for(int i = 0; i < n; i++) {
        solver.post(IntConstraintFactory.arithm(tmp[i], "<=", x[i]));
      }
    }

    if (show_all_opt) {
      solver.post(IntConstraintFactory.arithm(num_coins, "=", VariableFactory.fixed(opt_val, solver)));
    }

  }


  @Override
  public void createSolver() {
    solver = new Solver("Coins3");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(x));
  }

  @Override
  public void solve() {
    if (show_all_opt) {
      solver.findSolution();
    } else {
      solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, num_coins);
    }
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      do {
        System.out.println("num_coins: " + num_coins.getValue());
        for(int i = 0; i < n; i++) {
          System.out.print(x[i].getValue() + " ");         
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

    new Coins3().execute(args);

  }

}

