/**
 *
 * Channeling between a number and a digit array in Choco3
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
import solver.search.loop.monitors.SearchMonitorFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import util.ESat;
import util.tools.ArrayUtils;

public class ToNum extends AbstractProblem {

  @Option(name = "-n", usage = "Size of array (default 5).", required = false)
  int n = 5;

  @Option(name = "-base", usage = "Base (default 10).", required = false)
  int base = 10;

  @Option(name = "-extra", usage = "Use extra constraint (default false).", required = false)
  boolean extra = false;

  @Option(name = "-solutions", usage = "Number of solutions to show (default 0, all).", required = false)
  int solutions = 0;


  IntVar[] x;
  IntVar num; 

  /**
   *  toNum(a, num, base)
   *
   *  channelling between the array a and the number num (in base base).
   *
   */
  public void ToNum(IntVar[] a, IntVar num, int base) {

    int len = a.length;
    IntVar[] tmp = VariableFactory.boundedArray("tmp", len, 0, (int)Math.pow(base, len)-1, solver);
    for(int i = 0; i < len; i++) {
      solver.post(IntConstraintFactory.times(a[i],
                                             VariableFactory.fixed((int)Math.pow(base,len-i-1), solver), 
                                             tmp[i]));
    }
    solver.post(IntConstraintFactory.sum(tmp, num));

  }


  @Override
  public void buildModel() {

    int ub = (int)Math.pow(base, n)-1;
    System.out.println("upper bound: " + ub);
    x = VariableFactory.enumeratedArray("x", n, 0, base-1, solver);
    num = VariableFactory.bounded("z", 0, ub, solver);

    // The channeling between array x and the number num (in base base)
    ToNum(x, num, base);


    // some extra constraints, just for fun
    if (extra) {
      solver.post(IntConstraintFactory.alldifferent(x, "BC"));
    
      // second digit should be fixed
      solver.post(IntConstraintFactory.arithm(x[1],"=",VariableFactory.fixed(1,solver)));
    }

  }

  @Override
  public void createSolver() {
    solver = new Solver("ToNum");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.inputOrder_InDomainMin(x));
  }

  @Override
  public void solve() {
    // solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, z);
    solver.findSolution();

  }

  @Override
  public void prettyOut() {

    if(solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      do {
        
        System.out.println("base: " + base);
        System.out.println("num : " + num.getValue());
        System.out.print("x   : ");
        for(int i = 0; i < n; i++) {
          System.out.print(x[i].getValue() + " ");
        }
        System.out.println(" (shown in base 10)");

        num_solutions++;

        if (solutions > 0 && num_solutions >= solutions) {
          break;
        }

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

    new ToNum().execute(args);

  }
}

