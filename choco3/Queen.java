/**
 *
 * N-queens problem in Choco3.
 *
 * See http://en.wikipedia.org/wiki/Eight_queens_puzzle
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
import util.ESat;
import util.tools.ArrayUtils;

public class Queen extends AbstractProblem {

  @Option(name = "-n", usage = "n (size of problem, default 8).", required = false)
  int n = 8;

  @Option(name = "-solutions", usage = "number of solutions (default 0, all).", required = false)
  int solutions = 0;

  IntVar[] queens;


  @Override
  public void buildModel() {

    queens = VariableFactory.enumeratedArray("queens", n, 1, n, solver);
    IntVar[] diag1 = new IntVar[n];
    IntVar[] diag2 = new IntVar[n];

    for (int i = 0; i < n; i++) {
      diag1[i] = VariableFactory.offset(queens[i], i);
      diag2[i] = VariableFactory.offset(queens[i], -i);
    }

    solver.post(IntConstraintFactory.alldifferent(queens, "BC"));
    solver.post(IntConstraintFactory.alldifferent(diag1, "BC"));
    solver.post(IntConstraintFactory.alldifferent(diag2, "BC"));

  }

  @Override
  public void createSolver() {
    solver = new Solver("Queen");
  }

  @Override
  public void configureSearch() {
    
    solver.set(IntStrategyFactory.firstFail_InDomainMin(queens));
    // solver.set(IntStrategyFactory.firstFail_InDomainMiddle(queens));
    // solver.set(IntStrategyFactory.domOverWDeg_InDomainMin(queens, seed));
  }

  @Override
  public void solve() {
    // System.out.println(solver); // Solver/model before solve.
    solver.findSolution();
  }


  @Override
  public void prettyOut() {
 
    if (solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      do {
        for(int i = 0; i < n; i++) {
          System.out.print(queens[i].getValue() + " ");
        }
        System.out.println();
        num_solutions++;
        
        if (solutions > 0 && num_solutions >= solutions) {
          break;
        }

      } while (solver.nextSolution() == Boolean.TRUE);


      System.out.println("\nIt was " + num_solutions + " solutions.");

    }  else {
      System.out.println("No solution.");
    }

  }


  public static void main(String args[]) {

    new Queen().execute(args);

  }


}

