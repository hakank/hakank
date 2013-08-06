/**
 *
 * Magic sequence in Choco3.
 *
 * http://www.dcs.st-and.ac.uk/~ianm/CSPLib/prob/prob019/spec.html
 * """
 * A magic sequence of length n is a sequence of integers x0 . . xn-1 between 
 * 0 and n-1, such that for all i in 0 to n-1, the number i occurs exactly xi 
 * times in the sequence. For instance, 6,2,1,0,0,0,1,0,0,0 is a magic sequence 
 * since 0 occurs 6 times in it, 1 occurs twice, ...
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

public class MagicSequence extends AbstractProblem {

  @Option(name = "-n", usage = "Size of problem (default 10).", required = false)
  int n = 10;
 
  IntVar[] x;


  @Override
  public void buildModel() {
    
    int[] values = ArrayUtils.zeroToN(n);

    x = VariableFactory.enumeratedArray("x", n, 0, n-1, solver);

    boolean closed = true; // restricts domains of VARS to VALUES if set to true
    solver.post(IntConstraintFactory.global_cardinality(x, values, x, closed));
    
    // Redundant constraint
    solver.post(IntConstraintFactory.sum(x, VariableFactory.fixed(n, solver)));

  }

  @Override
  public void createSolver() {
    solver = new Solver("MagicSequence");
  }

  @Override
  public void configureSearch() {
    // solver.set(IntStrategyFactory.inputOrder_InDomainMin(x));
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

    new MagicSequence().execute(args);

  }

}

