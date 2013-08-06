/**
 *
 * Set covering problem in Choco3.
 *
 * This example is from the OPL example covering.mod
 * """
 * Consider selecting workers to build a house. The construction of a 
 * house can be divided into a number of tasks, each requiring a number of 
 * skills (e.g., plumbing or masonry). A worker may or may not perform a 
 * task, depending on skills. In addition, each worker can be hired for a 
 * cost that also depends on his qualifications. The problem consists of 
 * selecting a set of workers to perform all the tasks, while minimizing the 
 * cost. This is known as a set-covering problem. The key idea in modeling 
 * a set-covering problem as an integer program is to associate a 0/1 
 * variable with each worker to represent whether the worker is hired. 
 * To make sure that all the tasks are performed, it is sufficient to 
 * choose at least one worker by task. This constraint can be expressed by a 
 * simple linear inequality.
 * """
 *
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
import solver.search.strategy.IntStrategyFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import util.ESat;
import util.tools.ArrayUtils;

public class DivisibleBy9Through1 extends AbstractProblem {

  @Option(name = "-base", usage = "base (default 10).", required = false)
  int base = 10;

  int m;
  int n;

  IntVar[] x;
  IntVar[] t;


  /**
   *
   *  toNum(solver, a, num, base)
   *
   *  channelling between the array a and the number num
   *
   */
  private void toNum(IntVar[] a, IntVar num, int base) {
    int len = a.length;

    IntVar[] tmp = VariableFactory.boundedArray("tmp", len, 0, m, solver);
    for(int i = 0; i < len; i++) {
      //tmp[i] = solver.makeProd(a[i], (int)Math.pow(base,(len-i-1))).var();
      solver.post(IntConstraintFactory.times(a[i], 
                                             VariableFactory.fixed((int)Math.pow(base,(len-i-1)), solver),
                                             tmp[i]));
    }

    solver.post(IntConstraintFactory.sum(tmp, num));

  }

  @Override
  public void buildModel() {

    m = (int)Math.pow(base,(base-1)) - 1;
    n = base - 1;

    System.out.println("base: " + base + " m: " + m + " n: " + n);

    x = VariableFactory.enumeratedArray("x", n, 1, base-1, solver);

    // t[0] contains the answer
    t = VariableFactory.boundedArray("x", n, 0, m, solver);


    solver.post(IntConstraintFactory.alldifferent(x, "BC"));

    // Ensure the divisibility of base .. 1
    IntVar zero = VariableFactory.fixed(0, solver);
    for(int i = 0; i < n; i++) {
      int mm = base - i - 1;
      IntVar[] tt = VariableFactory.boundedArray("tt", mm, 0, m, solver);
      for(int j = 0; j < mm; j++) {
        tt[j] = x[j];
      }
      toNum(tt, t[i], base);
      IntVar mm_const = VariableFactory.fixed(mm, solver);
      solver.post(IntConstraintFactory.mod(t[i], mm_const, zero));

    }

  }

  @Override
  public void createSolver() {
    solver = new Solver("DivisibleBy9Through1");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(x));
  }

  @Override
  public void solve() {
    solver.findSolution();
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      do {

        System.out.print("x (base " + base + "): ");
        for(int i = 0; i < n; i++) {
            System.out.print(x[i].getValue() + "");
        }
        System.out.print("\nt (base 10): ");
        for(int i = 0; i < n; i++) {
            System.out.print(t[i].getValue() + " ");
        }
        System.out.println("\n");

        
      } while (solver.nextSolution() == Boolean.TRUE);


    }  else {
      System.out.println("No solution.");
    }

  }


  public static void main(String args[]) {

    new DivisibleBy9Through1().execute(args);

  }


}

