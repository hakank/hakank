/**
 *
 * Kakuro puzzle in Choco3.
 *
 * http://en.wikipedia.org/wiki/Kakuro
 * """
 * The object of the puzzle is to insert a digit from 1 to 9 inclusive
 * into each white cell such that the sum of the numbers in each entry
 * matches the clue associated with it and that no digit is duplicated in
 * any entry. It is that lack of duplication that makes creating Kakuro
 * puzzles with unique solutions possible, and which means solving a Kakuro
 * puzzle involves investigating combinations more, compared to Sudoku in
 * which the focus is on permutations. There is an unwritten rule for
 * making Kakuro puzzles that each clue must have at least two numbers
 * that add up to it. This is because including one number is mathematically
 * trivial when solving Kakuro puzzles; one can simply disregard the
 * number entirely and subtract it from the clue it indicates.
 * """
 *
 * This model solves the problem at the Wikipedia page.
 * For a larger picture, see
 * http://en.wikipedia.org/wiki/File:Kakuro_black_box.svg
 *
 * The solution:
 *  9 7 0 0 8 7 9
 *  8 9 0 8 9 5 7
 *  6 8 5 9 7 0 0
 *  0 6 1 0 2 6 0
 *  0 0 4 6 1 3 2
 *  8 9 3 1 0 1 4
 *  3 1 2 0 0 2 1
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

public class Kakuro extends AbstractProblem {

  
  // size of matrix
  int n = 7;
  
  // Segments:
  //    sum, the segments
  // Note: this is 1-based (fixed below)
  //
  int[][] problem =
  {
    new int[] {16,  1,1, 1,2},
    new int[] {24,  1,5, 1,6, 1,7},
    new int[] {17,  2,1, 2,2},
    new int[] {29,  2,4, 2,5, 2,6, 2,7},
    new int[] {35,  3,1, 3,2, 3,3, 3,4, 3,5},
    new int[] { 7,  4,2, 4,3},
    new int[] { 8,  4,5, 4,6},
    new int[] {16,  5,3, 5,4, 5,5, 5,6, 5,7},
    new int[] {21,  6,1, 6,2, 6,3, 6,4},
    new int[] { 5,  6,6, 6,7},
    new int[] { 6,  7,1, 7,2, 7,3},
    new int[] { 3,  7,6, 7,7},
    
    new int[] {23,  1,1, 2,1, 3,1},
    new int[] {30,  1,2, 2,2, 3,2, 4,2},
    new int[] {27,  1,5, 2,5, 3,5, 4,5, 5,5},
    new int[] {12,  1,6, 2,6},
    new int[] {16,  1,7, 2,7},
    new int[] {17,  2,4, 3,4},
    new int[] {15,  3,3, 4,3, 5,3, 6,3, 7,3},
    new int[] {12,  4,6, 5,6, 6,6, 7,6},
    new int[] { 7,  5,4, 6,4},
    new int[] { 7,  5,7, 6,7, 7,7},
    new int[] {11,  6,1, 7,1},
    new int[] {10,  6,2, 7,2}

  };
  

  int num_p = problem.length; // Number of segments
  
  // The blanks
  // Note: 1-based
  int[][] blanks = {
    {1,3}, {1,4},
    {2,3},
    {3,6}, {3,7},
    {4,1}, {4,4}, {4,7},
    {5,1}, {5,2},
    {6,5},
    {7,4}, {7,5}
  };
  
  int num_blanks = blanks.length;
   
  IntVar[][] x;


  @Override
  public void buildModel() {
    
    x = VariableFactory.enumeratedMatrix("x", n, n, 0, 9, solver);

    // fill the blanks with 0
    for(int i = 0; i < num_blanks; i++) {
      solver.post(IntConstraintFactory.arithm(x[blanks[i][0]-1][blanks[i][1]-1], "=", 0));
    }

    for(int i = 0; i < num_p; i++) {
      int[] segment = problem[i];

      // Remove the sum from the segment
      int[] s2 = new int[segment.length-1];
      for(int j = 1; j < segment.length; j++) {
        s2[j-1] = segment[j];
      }

      // all numbers in this segment must be distinct
      int len = segment.length / 2;
      IntVar[] t = VariableFactory.enumeratedArray("t", len, 1, 9, solver);
      for(int j = 0; j < len; j++) {
        t[j] = x[s2[j * 2] - 1][s2[j * 2 + 1] - 1];
        // ensure that the values are positive
        solver.post(IntConstraintFactory.arithm(t[j],">=", 1));
      }
      solver.post(IntConstraintFactory.alldifferent(t, "BC"));

      // sum the segment
      solver.post(IntConstraintFactory.sum(t, VariableFactory.fixed(segment[0], solver)));
    }


  }

  @Override
  public void createSolver() {
    solver = new Solver("Kakuro");
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
            int v = x[i][j].getValue();
            if (v > 0) {
              System.out.print(v + " ");
            } else {
              System.out.print("_ ");
            }
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

    new Kakuro().execute(args);

  }

}

