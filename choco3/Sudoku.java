/**
  *
  * Sudoku in Choco3.
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

public class Sudoku extends AbstractProblem {

  int[][] initial_grid;
  int cell_size;
  int n;

  IntVar[][] x;

  @Override
  public void createSolver() {
    solver = new Solver("Sudoku");
  }
  

  @Override
  public void buildModel() {    
    
    cell_size = 3;
    n = cell_size * cell_size;

    // 0 marks an unknown value
    initial_grid = new int[][] {{0, 6, 0, 0, 5, 0, 0, 2, 0},
                                {0, 0, 0, 3, 0, 0, 0, 9, 0},
                                {7, 0, 0, 6, 0, 0, 0, 1, 0},
                                {0, 0, 6, 0, 3, 0, 4, 0, 0},
                                {0, 0, 4, 0, 7, 0, 1, 0, 0},
                                {0, 0, 5, 0, 9, 0, 8, 0, 0},
                                {0, 4, 0, 0, 0, 1, 0, 0, 6},
                                {0, 3, 0, 0, 0, 8, 0, 0, 0},
                                {0, 2, 0, 0, 4, 0, 0, 5, 0}};
   
 
    x = VariableFactory.enumeratedMatrix("x", n, n, 1, 9, solver);
    
    // init, rows and columns
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        if (initial_grid[i][j] > 0) {
          solver.post(IntConstraintFactory.arithm(x[i][j], "=", initial_grid[i][j]));
        }
      }
      solver.post(IntConstraintFactory.alldifferent(x[i], "BC"));
      solver.post(IntConstraintFactory.alldifferent(ArrayUtils.getColumn(x, i), "BC"));
    }

    // cells
    for(int i = 0; i < cell_size; i++) {
      for(int j = 0; j < cell_size; j++) {
        IntVar[] cell = new IntVar[n];
        for(int di = 0; di < cell_size; di++) {
          for(int dj = 0; dj < cell_size; dj++) {
            cell[di * cell_size + dj] =
              x[i * cell_size + di][j * cell_size + dj];
          }
        }
        solver.post(IntConstraintFactory.alldifferent(cell, "BC"));
      }
    }
    
  }


  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.inputOrder_InDomainMin(ArrayUtils.flatten(x)));
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
        for (int i = 0; i < n; i++) {
          for (int j = 0; j < n; j++) {
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
    new Sudoku().execute(args);
  }


} // end class
 
