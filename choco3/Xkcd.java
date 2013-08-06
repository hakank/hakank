/**
 *
 * Xkcd Knapsack problem in Choco3.
 *
 * http://xkcd.com/287/
 *
 * Some amount (or none) of each dish should be ordered to give a 
 * total of exact 15.05.
 *
 * 
 * This Choco3 model was created by Hakan Kjellerstrand (hakank@bonetmail.com)
 * Also, see my Choco page: http://www.hakank.org/choco/ 
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
import util.ESat;
import util.tools.ArrayUtils;

import java.util.*;

public class Xkcd extends AbstractProblem {

  int n;
  int[] price;
  int total;

  IntVar[] x;


  @Override
  public void createSolver() {
    solver = new Solver("Xkcd");
  }
  

  @Override
  public void buildModel() {    
    
    // for price and total: multiplied by 100 to be able to use integers
    price = new int[] {215, 275, 335, 355, 420, 580};

    n = price.length;
    total = 1505;


    x = VariableFactory.enumeratedArray("x", n, 0, 10, solver);

    solver.post(IntConstraintFactory.scalar(x, price, VariableFactory.fixed(total, solver)));

  }


  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.inputOrder_InDomainMin(x));
  }
  
  @Override
  public void solve() {
    solver.findSolution();
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_sol = 0;
      do {

        System.out.println("Total: " + total);
        for (int i = 0; i < n; i++) {
            System.out.print(x[i].getValue() + " ");
        }

        System.out.println();
        num_sol++;

      } while (solver.nextSolution() == Boolean.TRUE);
      
      System.out.println("\nIt was " + num_sol + " solutions.");
      
    } else {
      System.out.println("No solution.");
    }

  }

  public static void main(String[] args) {
    new Xkcd().execute(args);
  }


} // end class
 
