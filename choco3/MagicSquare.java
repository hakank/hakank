/**
 *
 * Magic squares in Choco3.
 *
 * See 
 *   http://en.wikipedia.org/wiki/Magic_square
 *   http://mathworld.wolfram.com/MagicSquare.html
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

public class MagicSquare extends AbstractProblem {

  @Option(name = "-n", usage = "n (size of the grid, default 4).", required = false)
  int n = 4;

  @Option(name = "-solutions", usage = "number of solutions to show (default all, 0).", required = false)
  int solutions = 0;

  /*
   * From
   * http://en.wikipedia.org/wiki/Fr%C3%A9nicle_standard_form
   * """
   * A magic square is in Frénicle standard form, named for 
   * Bernard Frénicle de Bessy, if the following two conditions apply:
   *  - the element at position [1,1] (top left corner) is the smallest 
   *    of the four corner elements; and
   *  - the element at position [1,2] (top edge, second from left) is 
   *    smaller than the element in [2,1].
   * """
   */
  @Option(name = "-frenicle", usage = "Frénicle form symmetry breaking (default false).", required = false)
  boolean frenicle = false;
  
  // Decision variables
  IntVar[][] x;
  IntVar s;


  @Override
  public void buildModel() {

    x = VariableFactory.enumeratedMatrix("x", n, n, 1, n*n, solver);
    s = VariableFactory.fixed((n * (n * n + 1)) / 2, solver);

    IntVar[] diag1 = VariableFactory.enumeratedArray("x", n, 1, n*n, solver);
    IntVar[] diag2 = VariableFactory.enumeratedArray("x", n, 1, n*n, solver);

    // Rows and columns
    for(int i = 0; i < n; i++) {
      solver.post(IntConstraintFactory.sum(x[i], s));
      solver.post(IntConstraintFactory.sum(ArrayUtils.getColumn(x, i), s));

      diag1[i] = x[i][i];
      diag2[i] = x[i][n - i - 1];
    }

    // sum diagonals to s
    solver.post(IntConstraintFactory.sum(diag1, s));
    solver.post(IntConstraintFactory.sum(diag2, s));

    solver.post(IntConstraintFactory.alldifferent(ArrayUtils.flatten(x), "BC"));

    // Symmetry breaking (the -frenicle option)
    if (frenicle) {
      IntVar[] t = {x[0][0], x[0][n-1], x[n-1][0], x[n-1][n-1]};
      solver.post(IntConstraintFactory.minimum(x[0][0], t));
      solver.post(IntConstraintFactory.arithm(x[0][1], "<", x[1][0]));
    }

  }

  @Override
  public void createSolver() {
    solver = new Solver("MagicSquare(" + n + ")");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMiddle(ArrayUtils.flatten(x)));
    // solver.set(IntStrategyFactory.firstFail_InDomainMin(ArrayUtils.flatten(x)));
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
        System.out.println("Total: " + s.getValue());
        for(int i = 0; i < n; i++) {
          for(int j = 0; j < n; j++) {
            System.out.format("%3d", x[i][j].getValue());
          }
          System.out.println();
        }
        System.out.println("\n");
        num_solutions++;
        if (solutions > 0 && num_solutions >= solutions) {
          break;
        }
        
      } while (solver.nextSolution() == Boolean.TRUE);
      
      
    }  else {
      System.out.println("No solution.");
    }
    
  }


  public static void main(String args[]) {

    new MagicSquare().execute(args);

  }


}

