/**
 *
 * P-median problem in Choco3.
 *
 * Model and data from the OPL Manual, which describes the problem:
 * """
 * The P-Median problem is a well known problem in Operations Research.
 * The problem can be stated very simply, like this: given a set of customers
 * with known amounts of demand, a set of candidate locations for warehouses,
 * and the distance between each pair of customer-warehouse, choose P
 * warehouses to open that minimize the demand-weighted distance of serving
 * all customers from those P warehouses.
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

public class PMedian extends AbstractProblem {

  int p = 2;
  int num_customers = 4;

  int num_warehouses = 3;

  int[] demand = {100,80,80,70};
  int [][] distance = {{ 2, 10, 50},
                       { 2, 10, 52},
                       {50, 60,  3},
                       {40, 60,  1}};
  

  IntVar[] open;
  IntVar[][] ship;
  IntVar z;


  @Override
  public void buildModel() {

    open = VariableFactory.boundedArray("open", num_warehouses, 0, num_warehouses, solver);
    ship = VariableFactory.boundedMatrix("ship", num_customers, num_warehouses, 0, 1, solver);
    z = VariableFactory.bounded("z", 0, 1000, solver);

    IntVar[][] tmp = VariableFactory.boundedMatrix("tmp", num_customers, num_warehouses, 0, 1000, solver);
    for(int c = 0; c < num_customers; c++) {
      IntVar[] ship_c = VariableFactory.boundedArray("ship_c_"+c, num_warehouses, 0, 100, solver);
      for(int w = 0; w < num_warehouses; w++) {
        solver.post(IntConstraintFactory.times(ship[c][w],
                                               VariableFactory.fixed(demand[c]*distance[c][w],solver),
                                               tmp[c][w]));
        solver.post(IntConstraintFactory.arithm(ship[c][w], "<=", open[w]));

        ship_c[w] = ship[c][w];
      }
      solver.post(IntConstraintFactory.sum(ship_c, VariableFactory.fixed(1, solver)));
      
    }
    solver.post(IntConstraintFactory.sum(ArrayUtils.flatten(tmp), z));

    solver.post(IntConstraintFactory.sum(open, VariableFactory.fixed(p, solver)));
    
  }


  @Override
  public void createSolver() {
    solver = new Solver("PMedian");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(ArrayUtils.append(open, ArrayUtils.flatten(ship)))); 
  }

  @Override
  public void solve() {
    // solver.findSolution();
    solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, z);
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      do {
        System.out.println("z: " + z.getValue());
        System.out.println("ship:");
        for(int c = 0; c < num_customers; c++) {
          for(int w = 0; w < num_warehouses; w++) {
            System.out.format("%2d ", ship[c][w].getValue());
          }
          System.out.println();
        }
        System.out.println("open:");
        for(int w = 0; w < num_warehouses; w++) {
            System.out.format("%2d ", open[w].getValue());
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

    new PMedian().execute(args);

  }

}

