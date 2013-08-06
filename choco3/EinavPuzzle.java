/**
 *
 * Einav puzzle in Choco3
 *
 *
 * From
 * "A programming puzzle from Einav"
 * http://gcanyon.wordpress.com/2009/10/28/a-programming-puzzle-from-einav/
 * """
 * My friend Einav gave me this programming puzzle to work on. Given
 * this array of positive and negative numbers:
 * 33   30  -10 -6  18   7  -11 -23   6
 * ...
 * -25   4  16  30  33 -23  -4   4 -23
 *
 * You can flip the sign of entire rows and columns, as many of them
 * as you like. The goal is to make all the rows and columns sum to positive
 * numbers (or zero), and then to find the solution (there are more than one)
 * that has the smallest overall sum. So for example, for this array:
 * 33  30 -10
 * -16  19   9
 * -17 -12 -14
 * You could flip the sign for the bottom row to get this array:
 * 33  30 -10
 * -16  19   9
 * 17  12  14
 * Now all the rows and columns have positive sums, and the overall total is
 * 108.
 * But you could instead flip the second and third columns, and the second
 * row, to get this array:
 * 33  -30  10
 * 16   19    9
 * -17   12   14
 * All the rows and columns still total positive, and the overall sum is just
 * 66. So this solution is better (I don't know if it's the best)
 * A pure brute force solution would have to try over 30 billion solutions.
 * I wrote code to solve this in J. I'll post that separately.
 * """
 *
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
import solver.search.limits.FailLimit;
import solver.search.strategy.IntStrategyFactory;
import solver.search.loop.monitors.SearchMonitorFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import util.ESat;
import util.tools.ArrayUtils;

public class EinavPuzzle extends AbstractProblem {

   
   // Small problem
  // int rows = 3;
  // int cols = 3;
  // int[][] data = {
  //   { 33,  30, -10},
  //   {-16,  19,   9},
  //   {-17, -12, -14}
  // };
  
  
  // Full problem
  int rows = 27;
  int cols = 9;
  int[][] data = {{33,30,10,-6,18,-7,-11,23,-6},
                  {16,-19,9,-26,-8,-19,-8,-21,-14},
                  {17,12,-14,31,-30,13,-13,19,16},
                  {-6,-11,1,17,-12,-4,-7,14,-21},
                  {18,-31,34,-22,17,-19,20,24,6},
                  {33,-18,17,-15,31,-5,3,27,-3},
                  {-18,-20,-18,31,6,4,-2,-12,24},
                  {27,14,4,-29,-3,5,-29,8,-12},
                  {-15,-7,-23,23,-9,-8,6,8,-12},
                  {33,-23,-19,-4,-8,-7,11,-12,31},
                  {-20,19,-15,-30,11,32,7,14,-5},
                  {-23,18,-32,-2,-31,-7,8,24,16},
                  {32,-4,-10,-14,-6,-1,0,23,23},
                  {25,0,-23,22,12,28,-27,15,4},
                  {-30,-13,-16,-3,-3,-32,-3,27,-31},
                  {22,1,26,4,-2,-13,26,17,14},
                  {-9,-18,3,-20,-27,-32,-11,27,13},
                  {-17,33,-7,19,-32,13,-31,-2,-24},
                  {-31,27,-31,-29,15,2,29,-15,33},
                  {-18,-23,15,28,0,30,-4,12,-32},
                  {-3,34,27,-25,-18,26,1,34,26},
                  {-21,-31,-10,-13,-30,-17,-12,-26,31},
                  {23,-31,-19,21,-17,-10,2,-23,23},
                  {-3,6,0,-3,-32,0,-10,-25,14},
                  {-19,9,14,-27,20,15,-5,-27,18},
                  {11,-6,24,7,-17,26,20,-31,-25},
                  {-25,4,-16,30,33,23,-4,-4,23}};
  
  IntVar[][] x;
  IntVar[] x_flat;
  IntVar[] row_signs;
  IntVar[] col_signs;
  IntVar[] row_sums;
  IntVar[] col_sums;

  IntVar total_sum;


  @Override
  public void buildModel() {

    x = VariableFactory.boundedMatrix("x", rows, cols, -100, 100, solver);
    x_flat = ArrayUtils.flatten(x);

    total_sum = VariableFactory.bounded("total_sum", 0, 1000, solver);

    int[] signs_domain = {-1,1};
    // It would be nice with a variant where one can state the domains via
    // an array of possible values
    row_signs = VariableFactory.boundedArray("row_signs", rows, -1, 1, solver);
    col_signs = VariableFactory.boundedArray("col_signs", cols, -1, 1, solver);

    // Remove 0 from row_signs and col_signs
    IntVar zero = VariableFactory.fixed(0, solver);
    for(int i = 0; i < rows; i++) {
      solver.post(IntConstraintFactory.arithm(row_signs[i],"!=", zero));
    }
    for(int j = 0; j < cols; j++) {
      solver.post(IntConstraintFactory.arithm(col_signs[j],"!=", zero));
    }

    solver.post(IntConstraintFactory.sum(x_flat, total_sum));

    for(int i = 0; i < rows; i++) {
      for(int j = 0; j < cols; j++) {
        // x[i][j] == data[i][j] * row_signs[i] * col_signs[j]
        IntVar t = VariableFactory.bounded("t_"+i+"_"+j, -100, 100, solver);
        solver.post(IntConstraintFactory.times(row_signs[i], col_signs[j], t));
        solver.post(IntConstraintFactory.times(t, 
                                               VariableFactory.fixed(data[i][j], solver), 
                                               x[i][j]));
      }
    }

    row_sums = VariableFactory.boundedArray("row_sums", rows, 0, 1000, solver);
    for(int i = 0; i < rows; i++) {
      solver.post(IntConstraintFactory.sum(x[i], row_sums[i]));
    }

    col_sums = VariableFactory.boundedArray("col_sums", cols, 0, 1000, solver);
    for(int j = 0; j < cols; j++) {
      solver.post(IntConstraintFactory.sum(ArrayUtils.getColumn(x, j), col_sums[j]));
    }


  }

  @Override
  public void createSolver() {
    solver = new Solver("EinavPuzzle");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(ArrayUtils.append(col_signs,row_signs)));
  }

  @Override
  public void solve() {
    // solver.findSolution();
    solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, total_sum);
  }

  @Override
  public void prettyOut() {

    if(solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      do {
        
        System.out.println("total_sum: " + total_sum.getValue());
        System.out.println("x: ");
        for(int i = 0; i < rows; i++) {
          for(int j = 0; j < cols; j++) {
            System.out.format("%3d ", x[i][j].getValue());
          }
          System.out.println();
        }
        System.out.println();
        System.out.print("row_sums: ");
        for(int i = 0; i < rows; i++) {
          System.out.print(row_sums[i].getValue() + " ");
        }
        System.out.println();
        System.out.print("col_sums: ");
        for(int j = 0; j < cols; j++) {
          System.out.print(col_sums[j].getValue() + " ");
        }
        System.out.println();


        System.out.print("row_signs: ");
        for(int i = 0; i < rows; i++) {
          System.out.print(row_signs[i].getValue() + " ");
        }
        System.out.println();
        System.out.print("col_signs: ");
        for(int j = 0; j < cols; j++) {
          System.out.print(col_signs[j].getValue() + " ");
        }
        System.out.println();
        System.out.println();

        num_solutions++;

      } while (solver.nextSolution() == Boolean.TRUE);

      System.out.println("It was " + num_solutions + " solutions.");

    } else {

      System.out.println("Problem is not feasible.");

    }

  }


  //
  // main
  //
  public static void main(String args[]) {

    new EinavPuzzle().execute(args);

  }
}

