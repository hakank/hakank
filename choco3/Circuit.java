/**
 *
 * Implements decompositions of global constraints circuit and circuit_path in Choco3.
 *
 * This model implements a decomposition of global constraint circuit.
 * Note: Choco3 has already support for circuit so this should be 
 *       considered an etude.
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
import solver.search.strategy.IntStrategyFactory;
import solver.search.strategy.selectors.values.InDomainMax;
import solver.search.strategy.selectors.values.InDomainMiddle;
import solver.search.strategy.selectors.values.InDomainMin;
import solver.search.strategy.selectors.values.InDomainRandom;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.explanations.ExplanationFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import solver.search.strategy.selectors.variables.*;
import solver.search.strategy.strategy.Assignment;
import util.ESat;
import util.tools.ArrayUtils;

import java.util.*;

public class Circuit extends AbstractProblem {

  @Option(name = "-n", usage = "Size of the array (default 5).", required = false)
  int n = 5;

  IntVar[] x;
  IntVar[] path;


  /**
   * circuit(x)
   *
   * A decomposition of the global constraint circuit, based
   * on some observation of the orbits in an array.
   *
   * Note: The domain of x must be 0..n-1 (not 1..n)
   * since Java is 0-based.
   */
  public void circuit(IntVar[] x) {

    IntVar[] z = VariableFactory.enumeratedArray("z", n, 0, n-1, solver);
    circuit_path(x, z);
  }


  /**
   * circuit_path(x, path)
   *
   * A decomposition of the global constraint circuit, based
   * on some observation of the orbits in an array.
   * 
   * path is the visited path in the circuit, starting from first 
   * element.
   *
   * Note: The domain of x must be 0..n-1 (not 1..n)
   * since Java is 0-based.
   */
  public void circuit_path(IntVar[] x, IntVar[] z) {

    int n = x.length;   

    solver.post(IntConstraintFactory.alldifferent(x, "BC"));
    solver.post(IntConstraintFactory.alldifferent(z, "BC"));

    // put the orbit of x[0] in z[0..n-1]
    solver.post(IntConstraintFactory.arithm(z[0],"=",x[0]));
    for(int i = 1; i < n-1; i++) {
      // z[i] = x[z[i-1]]
      solver.post(IntConstraintFactory.element(z[i], x, z[i-1], 0));
    }

    // z may not be 0 for i < n-1
    IntVar zero = VariableFactory.fixed(0, solver);
    for(int i = 1; i < n - 1; i++) {
      solver.post(IntConstraintFactory.arithm(z[i], "!=", zero));
    }

    // when i = n-1 it must be 0
    solver.post(IntConstraintFactory.arithm(z[n-1], "=", zero));

  }



  @Override
  public void buildModel() {

    x = VariableFactory.enumeratedArray("x", n, 0, n-1, solver);
    path = VariableFactory.enumeratedArray("path", n, 0, n-1, solver);

    circuit_path(x, path);

  }


  @Override
  public void createSolver() {
    solver = new Solver("Circuit");
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
        System.out.print("x   : ");
        for(int i = 0; i < n; i++) {
            System.out.format("%d ", x[i].getValue());
        }
        System.out.println();
        System.out.print("path: ");
        for(int i = 0; i < n; i++) {
            System.out.format("%d ", path[i].getValue());
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

    new Circuit().execute(args);

  }

}

