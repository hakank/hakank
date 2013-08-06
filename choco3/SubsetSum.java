/**
 *
 * Subset sum problem in Choco3
 *
 * From Katta G. Murty: 'Optimization Models for Decision Making', page 340
 * http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
 * """
 * Example 7.8.1
 * 
 * A bank van had several bags of coins, each containing either
 * 16, 17, 23, 24, 39, or 40 coins. While the van was parked on the
 * street, thieves stole some bags. A total of 100 coins were lost.
 * It is required to find how many bags were stolen.
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

public class SubsetSum extends AbstractProblem {

  @Option(name = "-total", usage = "The total (default 100).", required = false)
  int total = 100;

  @Option(name = "-minimize", usage = "Minimize the number of bags (default false).", required = false)
  boolean minimize = false;


  int[] coins = {16, 17, 23, 24, 39, 40};

  int n = coins.length;

  IntVar[] x;
  IntVar z; // number of bags taken

  @Override
  public void buildModel() {


    x = VariableFactory.boundedArray("x", n, 0, total, solver);
    z = VariableFactory.bounded("z", 0, total, solver);

    solver.post(IntConstraintFactory.sum(x, z));
    solver.post(IntConstraintFactory.scalar(x, coins, VariableFactory.fixed(total, solver)));

  }

  @Override
  public void createSolver() {
    solver = new Solver("SubsetSum");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.maxReg_InDomainMin(x));
  }

  @Override
  public void solve() {
    if (minimize) {
      solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, z);
    } else {
      solver.findSolution();
    }

  }

  @Override
  public void prettyOut() {

    if(solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      do {
        
        System.out.println("total: " + total);
        System.out.println("num bags: " + z.getValue());
        for(int i = 0; i < n; i++) {
          System.out.format("%3d bag(s) with %2d coins\n", x[i].getValue(), coins[i]);
        }
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

    new SubsetSum().execute(args);

  }
}

