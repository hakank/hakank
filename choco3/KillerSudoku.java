/**
 *
 * Killer Sudoku in Choco3.
 *
 * http://en.wikipedia.org/wiki/Killer_Sudoku
 * """
 * Killer sudoku (also killer su doku, sumdoku, sum doku, addoku, or
 * samunamupure) is a puzzle that combines elements of sudoku and kakuro.
 * Despite the name, the simpler killer sudokus can be easier to solve
 * than regular sudokus, depending on the solver's skill at mental arithmetic;
 * the hardest ones, however, can take hours to crack.
 *
 * ...
 *
 * The objective is to fill the grid with numbers from 1 to 9 in a way that
 * the following conditions are met:
 *
 * - Each row, column, and nonet contains each number exactly once.
 * - The sum of all numbers in a cage must match the small number printed
 *   in its corner.
 * - No number appears more than once in a cage. (This is the standard rule
 *   for killer sudokus, and implies that no cage can include more
 *   than 9 cells.)
 *
 * In 'Killer X', an additional rule is that each of the long diagonals
 * contains each number once.
 * """
 *
 * Here we solve the problem from the Wikipedia page, also shown here
 * http://en.wikipedia.org/wiki/File:Killersudoku_color.svg
 *
 * The output is:
 *   2 1 5 6 4 7 3 9 8
 *   3 6 8 9 5 2 1 7 4
 *   7 9 4 3 8 1 6 5 2
 *   5 8 6 2 7 4 9 3 1
 *   1 4 2 5 9 3 8 6 7
 *   9 7 3 8 1 6 4 2 5
 *   8 2 1 7 3 9 5 4 6
 *   6 5 9 4 2 8 7 1 3
 *   4 3 7 1 6 5 2 8 9
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
import solver.search.strategy.IntStrategyFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import solver.search.strategy.selectors.variables.*;
import util.ESat;
import util.tools.ArrayUtils;

public class KillerSudoku extends AbstractProblem {


  // size of matrix
  int cell_size = 3;
  int n = cell_size*cell_size;
  
  // For a better view of the problem, see
  //  http://en.wikipedia.org/wiki/File:Killersudoku_color.svg
  
  // hints
  //  sum, the hints
  //
  // Note: this is 1-based (will be fixed below)
  //
  int[][] problem =
  {
    new int[] { 3,  1,1,  1,2},
    new int[] {15,  1,3,  1,4, 1,5},
    new int[] {22,  1,6,  2,5, 2,6, 3,5},
    new int[] {4,   1,7,  2,7},
    new int[] {16,  1,8,  2,8},
    new int[] {15,  1,9,  2,9, 3,9, 4,9},
    new int[] {25,  2,1,  2,2, 3,1, 3,2},
    new int[] {17,  2,3,  2,4},
    new int[] { 9,  3,3,  3,4, 4,4},
    new int[] { 8,  3,6,  4,6, 5,6},
    new int[] {20,  3,7,  3,8, 4,7},
    new int[] { 6,  4,1,  5,1},
    new int[] {14,  4,2,  4,3},
    new int[] {17,  4,5,  5,5, 6,5},
    new int[] {17,  4,8,  5,7, 5,8},
    new int[] {13,  5,2,  5,3, 6,2},
    new int[] {20,  5,4,  6,4, 7,4},
    new int[] {12,  5,9,  6,9},
    new int[] {27,  6,1,  7,1, 8,1, 9,1},
    new int[] { 6,  6,3,  7,2, 7,3},
    new int[] {20,  6,6,  7,6, 7,7},
    new int[] { 6,  6,7,  6,8},
    new int[] {10,  7,5,  8,4, 8,5, 9,4},
    new int[] {14,  7,8,  7,9, 8,8, 8,9},
    new int[] { 8,  8,2,  9,2},
    new int[] {16,  8,3,  9,3},
    new int[] {15,  8,6,  8,7},
    new int[] {13,  9,5,  9,6, 9,7},
    new int[] {17,  9,8,  9,9}
    
  };
 
  int num_p = 29; // Number of segments
   
  IntVar[][] x;


  @Override
  public void buildModel() {
    
    x = VariableFactory.enumeratedMatrix("x", n, n, 0, 9, solver);


    // init, rows and columns
    for(int i = 0; i < n; i++) {
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

    // Sum the segments and ensure alldifferent
    for(int i = 0; i < num_p; i++) {
      int[] segment = problem[i];

      // Remove the sum from the segment
      int[] s2 = new int[segment.length-1];
      for(int j = 1; j < segment.length; j++) {
        s2[j-1] = segment[j];
      }

      // all numbers in this segment must be distinct
      int len = segment.length / 2;
      IntVar[] t = VariableFactory.enumeratedArray("t", len, 0, 9, solver);
      for(int j = 0; j < len; j++) {
        t[j] = x[s2[j*2]-1][s2[j*2+1]-1];
      }
      solver.post(IntConstraintFactory.alldifferent(t, "BC"));
      
      // Ensure that the sum of the segments = segment[0]
      solver.post(IntConstraintFactory.sum(t, VariableFactory.fixed(segment[0], solver)));
    }

  }

  @Override
  public void createSolver() {
    solver = new Solver("KillerSudoku");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(ArrayUtils.flatten(x)));
  }

  @Override
  public void solve() {
    solver.findSolution();
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      do {

        for(int i = 0; i < n; i++) {
          for(int j = 0; j < n; j++) {
            System.out.print(x[i][j].getValue() + " ");
          }
          System.out.println();
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

    new KillerSudoku().execute(args);

  }

}

