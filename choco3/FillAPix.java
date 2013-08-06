/**
 *
 * Fill-a-Pix problem in Choco3.
 *
 * From http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/basiclogic
 * """
 * Each puzzle consists of a grid containing clues in various places. The
 * object is to reveal a hidden picture by painting the squares around each
 * clue so that the number of painted squares, including the square with
 * the clue, matches the value of the clue.
 * """
 *
 * http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
 * """
 * Fill-a-Pix is a Minesweeper-like puzzle based on a grid with a pixilated
 * picture hidden inside. Using logic alone, the solver determines which
 * squares are painted and which should remain empty until the hidden picture
 * is completely exposed.
 * """
 *
 * Fill-a-pix History:
 * http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/history
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

import java.util.*;

public class FillAPix extends AbstractProblem {

  @Option(name = "-d", aliases = "--data", usage = "Problem instance (default 1)", required = false)
  Data data = Data.p1;


  static int X = -1;

  int n;
  int[][] puzzle;
  
  IntVar[][] x;

  @Override
  public void buildModel() {
    
   int[] S = {-1, 0, 1};

   puzzle = data.getProblem();
   n = puzzle.length;

    // Decision variables
    x = VariableFactory.enumeratedMatrix("x", n, n, 0, 1, solver);

    // Constraints
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        if (puzzle[i][j] > X) {
          ArrayList<IntVar> neighbours = new ArrayList<IntVar>();
          for(int a: S) {
            for(int b: S) {
              if (i+a >= 0 && j+b >=  0 && i+a < n && j+b < n) {
                neighbours.add(x[i+a][j+b]);
              }
            }                        
          }

          solver.post(IntConstraintFactory.sum(neighbours.toArray(new IntVar[1]), 
                                               VariableFactory.fixed(puzzle[i][j], solver)));

        }
      }
    }


  }

  @Override
  public void createSolver() {
    solver = new Solver("FillAPix");
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
            if (x[i][j].getValue() == 1) {
              System.out.print("# ");
            } else {
              System.out.print("  ");
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

    new FillAPix().execute(args);

  }


  static enum Data {
    
    // Puzzle 1 from 
    // http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
    p1(new int[][] {{X,X,X,X,X,X,X,X,0,X},
                    {X,8,8,X,2,X,0,X,X,X},
                    {5,X,8,X,X,X,X,X,X,X},
                    {X,X,X,X,X,2,X,X,X,2},
                    {1,X,X,X,4,5,6,X,X,X},
                    {X,0,X,X,X,7,9,X,X,6},
                    {X,X,X,6,X,X,9,X,X,6},
                    {X,X,6,6,8,7,8,7,X,5},
                    {X,4,X,6,6,6,X,6,X,4},
                    {X,X,X,X,X,X,3,X,X,X}}
       ),


      // Puzzle 2 from 
      // http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
      p2(new int[][] {{0,X,X,X,X,X,3,4,X,3},
                      {X,X,X,4,X,X,X,7,X,X},
                      {X,X,5,X,2,2,X,4,X,3},
                      {4,X,6,6,X,2,X,X,X,X},
                      {X,X,X,X,3,3,X,X,3,X},
                      {X,X,8,X,X,4,X,X,X,X},
                      {X,9,X,7,X,X,X,X,5,X},
                      {X,X,X,7,5,X,X,3,3,0},
                      {X,X,X,X,X,X,X,X,X,X},
                      {4,4,X,X,2,3,3,4,3,X}}
         ),

      // Puzzle from 
      // http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/basiclogic
      p3(new int[][] {{X,5,X,6,X,X,X,X,X,X,6,X,X,X,X},
                      {X,X,7,6,X,4,X,X,4,X,X,8,9,X,5},
                      {5,X,X,5,X,5,X,3,X,6,X,7,X,X,6},
                      {4,X,2,X,4,X,4,X,3,X,2,X,X,9,X},
                      {X,X,X,5,X,4,X,3,X,4,X,4,5,X,6},
                      {X,4,3,3,4,X,X,X,4,X,2,X,X,X,X},
                      {X,X,X,X,X,X,X,X,X,5,X,X,X,4,X},
                      {3,X,3,X,X,3,X,X,X,5,X,4,4,X,X},
                      {X,X,X,4,3,X,3,3,X,X,5,7,6,X,X},
                      {4,X,X,X,2,X,3,3,2,X,8,9,X,5,X},
                      {X,X,3,X,X,X,X,5,X,X,7,X,8,X,X},
                      {4,X,X,3,2,X,X,X,X,X,7,X,X,6,X},
                      {X,X,4,X,5,4,4,X,X,9,6,X,X,X,X},
                      {X,3,5,7,X,6,X,X,X,X,X,X,7,X,X},
                      {X,X,4,6,6,X,X,X,6,5,X,X,X,4,X}}
        );

    final int[][] data;

    Data(int[][] data) {
      this.data = data;
    }
    
    int[][] getProblem() {
      return data;
    }


  }

}

