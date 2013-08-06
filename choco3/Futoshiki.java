/**
 *
 * Futoshiki problem in Choco3.
 *
 * From http://en.wikipedia.org/wiki/Futoshiki
 * """
 * The puzzle is played on a square grid, such as 5 x 5. The objective
 * is to place the numbers 1 to 5 (or whatever the dimensions are)
 * such that each row, and column contains each of the digits 1 to 5.
 * Some digits may be given at the start. In addition, inequality
 * constraints are also initially specifed between some of the squares,
 * such that one must be higher or lower than its neighbour. These
 * constraints must be honoured as the grid is filled out.
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
import solver.constraints.IntConstraintFactory.*;
import solver.search.strategy.IntStrategyFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import solver.search.strategy.selectors.variables.*;
import util.ESat;
import util.tools.ArrayUtils;

public class Futoshiki extends AbstractProblem {

  @Option(name = "-problem", usage = "problem instance (default 1).", required = false)
  int problem = 1;


  //
  // Example from Tailor model futoshiki.param/futoshiki.param
  // Solution:
  // 5 1 3 2 4
  // 1 4 2 5 3
  // 2 3 1 4 5
  // 3 5 4 1 2
  // 4 2 5 3 1
  //
  // Futoshiki instance, by Andras Salamon
  // specify the numbers in the grid
  //
  int[][] values1 = {{0, 0, 3, 2, 0},
                     {0, 0, 0, 0, 0},
                     {0, 0, 0, 0, 0},
                     {0, 0, 0, 0, 0},
                     {0, 0, 0, 0, 0}};
  // [i1,j1, i2,j2] requires that values[i1,j1] < values[i2,j2]
  // Note: 1-based
  int [][] lt1 = {{1,2,  1,1},
                  {1,4,  1,5},
                  {2,3,  1,3},
                  {3,3,  2,3},
                  {3,4,  2,4},
                  {2,5,  3,5},
                  {3,2,  4,2},
                  {4,4,  4,3},
                  {5,2,  5,1},
                  {5,4,  5,3},
                  {5,5,  4,5}};
  

  //
  // Example from http://en.wikipedia.org/wiki/Futoshiki
  // Solution:
  // 5 4 3 2 1
  // 4 3 1 5 2
  // 2 1 4 3 5
  // 3 5 2 1 4
  // 1 2 5 4 3
  //
  int[][] values2 = {{0, 0, 0, 0, 0},
                     {4, 0, 0, 0, 2},
                     {0, 0, 4, 0, 0},
                     {0, 0, 0, 0, 4},
                     {0, 0, 0, 0, 0}};
  // Note: 1-based
  int[][] lt2 = {{1,2,  1,1},
                 {1,4,  1,3},
                 {1,5,  1,4},
                 {4,4,  4,5},
                 {5,1,  5,2},
                 {5,2,  5,3}};
  

  int[][] values;
  int[][] lt;
  
  int size;

  IntVar[][] x;


  @Override
  public void buildModel() {
    
    if (problem == 2) {
      values = values2;
      lt = lt2;
    } else {
      values = values1;
      lt = lt1;
    }

    size = values.length;

    x = VariableFactory.enumeratedMatrix("x", size, size, 1, size, solver);

    // initial values and alldifferent rows/column
    for(int i = 0; i < size; i++) {
      for(int j = 0; j < size; j++) {
        if (values[i][j] > 0) {
          solver.post(IntConstraintFactory.arithm(x[i][j], 
                                                  "=", 
                                                  VariableFactory.fixed(values[i][j], solver)));
        }
      }

      solver.post(IntConstraintFactory.alldifferent(x[i], "BC"));
      solver.post(IntConstraintFactory.alldifferent(ArrayUtils.getColumn(x,i), "BC"));

    }


    // ensure that all < constraints are satisfied
    // Also: make 0-based.
    for(int i = 0; i < lt.length; i++) {
      solver.post(IntConstraintFactory.arithm(x[ lt[i][0]-1][lt[i][1]-1 ], 
                                              "<",
                                              x[ lt[i][2]-1][lt[i][3]-1 ]));
    }
    

  }

  @Override
  public void createSolver() {
    solver = new Solver("Futoshiki");
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

        for(int i = 0; i < size; i++) {
          for(int j = 0; j < size; j++) {
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

    new Futoshiki().execute(args);

  }

}

