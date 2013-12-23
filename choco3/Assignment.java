/**
  *
  * Assignment problem in Choco3.
  *
  * Winston "Operations Research", Assignment Problems, page 393f
  *
  * (generalized version with added test column)
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
import solver.search.strategy.IntStrategyFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import util.ESat;
import util.tools.ArrayUtils;

import java.util.*;

public class Assignment extends AbstractProblem {

  int[][] cost;
  int rows; 
  int cols;

  IntVar[][] x;
  IntVar total_cost;


  @Override
  public void createSolver() {
    solver = new Solver("Assignment");
  }
  

  @Override
  public void buildModel() {    

    cost = new int[][] {{14,  5, 8,  7, 15},
                        { 2, 12, 6,  5,  3},
                        { 7,  8, 3,  9,  7},
                        { 2,  4, 6, 10,  1}};

    rows = 4;
    cols = 5;
    
    x = VariableFactory.enumeratedMatrix("x", rows, cols, 0, 1, solver);
    total_cost = VariableFactory.bounded("total_cost", 0, 1000, solver);

    IntVar one = VariableFactory.fixed(1, solver);

    // Rows and total
    IntVar[][] totals = VariableFactory.boundedMatrix("totals", rows, cols, 0, 1000, solver);
    for(int i = 0; i < rows; i++) {
      for(int j = 0; j < cols; j++) {
        solver.post(IntConstraintFactory.times(x[i][j], VariableFactory.fixed(cost[i][j], solver),totals[i][j]));
      }

      solver.post(IntConstraintFactory.sum(x[i], one));

    }

    solver.post(IntConstraintFactory.sum(ArrayUtils.flatten(totals), total_cost));

    // Columns
    for(int j = 0; j < cols; j++) {
      IntVar col_sum = VariableFactory.bounded("col_sum", 0, rows, solver);
      solver.post(IntConstraintFactory.sum(ArrayUtils.getColumn(x, j), col_sum));
      solver.post(IntConstraintFactory.arithm(col_sum,"<=", one));

    }

  }


  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(ArrayUtils.flatten(x)));
  }
  
  @Override
  public void solve() {
    solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, total_cost);
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_sol = 0;
      do {
        System.out.println("total_cost: " + total_cost.getValue());
        for (int i = 0; i < rows; i++) {
          for(int j = 0; j < cols; j++) {
            System.out.print(x[i][j].getValue() + " ");
          }
          System.out.println();
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
    new Assignment().execute(args);
  }


} // end class
 
