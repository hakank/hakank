/**
 *
 * Lectures problem in Choco3.
 *
 * Biggs: Discrete Mathematics (2nd ed), page 187.
 * """
 * Suppose we wish to schedule six one-hour lectures, v1, v2, v3, v4, v5, v6.
 * Among the the potential audience there are people who wish to hear both
 *
 * - v1 and v2
 * - v1 and v4
 * - v3 and v5
 * - v2 and v6
 * - v4 and v5
 * - v5 and v6
 * - v1 and v6
 *
 * How many hours are necessary in order that the lectures can be given
 * without clashes?
 * """
 *
 * Note: This can be seen as a coloring problem.
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
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.explanations.ExplanationFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import solver.search.strategy.selectors.variables.*;
import util.ESat;
import util.tools.ArrayUtils;

import java.util.*;

public class Lectures extends AbstractProblem {

  //
  // The schedule requirements:
  // lecture a cannot be held at the same time as b
  // Note: 1-based (compensated in the constraints).
  int[][] g = 
  {
    {1, 2},
    {1, 4},
    {3, 5},
    {2, 6},
    {4, 5},
    {5, 6},
    {1, 6}
  };
  
  // number of nodes
  int n = 6;

  // number of edges
  int edges = g.length;


  IntVar[] x;
  // maximum color (hour) to minimize
  // Note: since Java is 0-based, the
  // number of colors is max_c + 1.
  IntVar max_c; 


  @Override
  public void buildModel() {
  
    x = VariableFactory.enumeratedArray("x", n, 0, n-1, solver);

    max_c = VariableFactory.enumerated("x", 0, n, solver);
    solver.post(IntConstraintFactory.maximum(max_c, x));

    // Ensure that there are no clashes.
    // Also, adjust to 0-base.
    for(int i = 0; i < edges; i++) {
      solver.post(IntConstraintFactory.arithm(x[g[i][0]-1], "!=", x[g[i][1]-1]));
    }
    
    // Symmetry breaking:
    // - v0 has the color 0,
    // - v1 has either color 0 or 1
    solver.post(IntConstraintFactory.arithm(x[0],"=", VariableFactory.fixed(0, solver)));
    solver.post(IntConstraintFactory.arithm(x[1],"<=", VariableFactory.fixed(1, solver)));

  }


  @Override
  public void createSolver() {
    solver = new Solver("Lectures");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.maxReg_InDomainMin(x));
  }

  @Override
  public void solve() {
    // solver.findSolution();
    solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, max_c);
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      do {
        System.out.println("number of hours: " + (1+max_c.getValue()));
        for(int i = 0; i < n; i++) {
          System.out.format("Lecture %d at %d h\n", i, x[i].getValue());
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

    new Lectures().execute(args);

  }

}

