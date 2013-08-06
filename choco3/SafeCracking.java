/**
 *
 * Safe cracking problem in Choco3.
 *
 * From the Oz Primer:
 * http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/index.html
 * """
 * The code of Professor Smart's safe is a sequence of 9 distinct
 * nonzero digits C1 .. C9 such that the following equations and
 * inequations are satisfied:
 *
 *       C4 - C6   =   C7
 *  C1 * C2 * C3   =   C8 + C9
 *  C2 + C3 + C6   <   C8
 *            C9   <   C8
 *
 *  and
 *
 *  C1 <> 1, C2 <> 2, ..., C9 <> 9
 *
 * can you find the correct combination?
 * """
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

public class SafeCracking extends AbstractProblem {

  int n = 9;

  IntVar[] x;


  @Override
  public void buildModel() {
    
    x = VariableFactory.enumeratedArray("x", n, 1, n, solver);
    IntVar c1 = x[0];
    IntVar c2 = x[1];
    IntVar c3 = x[2];
    IntVar c4 = x[3];
    IntVar c5 = x[4];
    IntVar c6 = x[5];
    IntVar c7 = x[6];
    IntVar c8 = x[7];
    IntVar c9 = x[8];


    solver.post(IntConstraintFactory.alldifferent(x, "BC"));

    // c4 - c6 = c7
    solver.post(IntConstraintFactory.sum(new IntVar[] {c4,VariableFactory.minus(c6)}, c7));

    // c1 * c2 * c3 == c8 + c9);
    IntVar c1c2 = VariableFactory.bounded("c1c2", 1, n*n, solver);
    solver.post(IntConstraintFactory.times(c1,c2, c1c2));
    IntVar c1c2c3 = VariableFactory.bounded("c1c2c3", 1, n*n*n, solver);
    solver.post(IntConstraintFactory.times(c1c2,c3, c1c2c3));

    IntVar c8c9 = VariableFactory.bounded("c8c9", 1, 2*n, solver);
    solver.post(IntConstraintFactory.sum(new IntVar[] {c8,c9},c8c9));
    solver.post(IntConstraintFactory.arithm(c1c2c3,"=",c8c9));
    
    // c2 + c3 + c6 < c8
    IntVar c2c3c6 = VariableFactory.bounded("c2c3c6", 1, 3*n, solver);
    solver.post(IntConstraintFactory.sum(new IntVar[] {c2,c3,c6}, c2c3c6));
    solver.post(IntConstraintFactory.arithm(c2c3c6,"<",c8));

    // c9 < c8
    solver.post(IntConstraintFactory.arithm(c9,"<", c8));

    for(int i = 1; i <= n; i++) {
      solver.post(IntConstraintFactory.arithm(x[i-1],"!=", i));
    }


  }

  @Override
  public void createSolver() {
    solver = new Solver("SafeCracking");
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
      int num_solutions = 0;
      do {
        System.out.print("x  : ");
        for(int i = 0; i < n; i++) {
          System.out.print(x[i].getValue() + " ");
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

    new SafeCracking().execute(args);

  }


}

