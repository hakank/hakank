/**
 *
 * 17x17 problem in Choco3
 *
 * Problem from Karsten Konrad:
 * http://lookforlight.tumblr.com/post/996786415/lets-do-real-cp-forbiddenassignment
 * """
 * The n x m grid is c-colorable if there is a way
 * to c-color the vertices of the n x m grid so that
 * there is no rectangle with all four corners the
 * same color. (The rectangles I care about have the
 * sides parallel to the x and y axis.)
 * 
 * Is there a 17x17 solution?
 * see: http://blog.computationalcomplexity.org/2009/11/17x17-challenge-worth-28900-this-is-not.html
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


public class Grid17x17 extends AbstractProblem {

  @Option(name = "-n", usage = "Size of matrix (default 13x13).", required = false)
  int n = 13;

  @Option(name = "-num_colors", usage = "Number of colors (default 4).", required = false)
  int num_colors = 4;

  @Option(name = "-solutions", usage = "Number of solutions to show (default 1).", required = false)
  int solutions = 1;

  @Option(name = "-symmetry", usage = "Symmetry breaking (default false).", required = false)
  boolean symmetry = false;


  IntVar[][] x;

  @Override
  public void buildModel() {

    x = VariableFactory.enumeratedMatrix("x", n, n, 1, num_colors, solver);

    int[] values = ArrayUtils.oneToN(num_colors);
    boolean closed = true;
    for(int r = 0; r < n; r++) {
      for(int r2 = 0; r2 < r; r2++) {
        for(int c = 0; c < n; c++) {
          for(int c2 = 0; c2 < c; c2++) {
            IntVar[] tmp = new IntVar[] {x[r][c],
                                         x[r2][c],
                                         x[r][c2],
                                         x[r2][c2]};
            IntVar[] gcc = VariableFactory.enumeratedArray("gcc", 4, 0, 3, solver);
            solver.post(IntConstraintFactory.global_cardinality(tmp, values, gcc, closed));
          }
        }
      }
    }
    
    if (symmetry) {
      solver.post(IntConstraintFactory.arithm(x[0][0], "=", VariableFactory.fixed(1, solver)));
      // solver.post(IntConstraintFactory.arithm(x[n-1][n-1], "=", VariableFactory.fixed(2, solver)));
    }
      


  }

  @Override
  public void createSolver() {
    solver = new Solver("Grid17x17");
  }

  @Override
  public void configureSearch() {
    // solver.set(IntStrategyFactory.firstFail_InDomainMin(ArrayUtils.flatten(x)));
    // solver.set(IntStrategyFactory.random(ArrayUtils.flatten(x), seed));
    // solver.set(IntStrategyFactory.domOverWDeg_InDomainMin(ArrayUtils.flatten(x), seed));

    // solver.set(IntStrategyFactory.firstFail_InDomainMin(ArrayUtils.flatten(x)));
    // solver.set(IntStrategyFactory.firstFail_InDomainMax(ArrayUtils.flatten(x))); 
    // solver.set(IntStrategyFactory.firstFail_InDomainMiddle(ArrayUtils.flatten(x)));
    // solver.set(IntStrategyFactory.firstFail_InDomainMiddle(ArrayUtils.append(ArrayUtils.flatten(x), counts)));
    // solver.set(new Assignment(new DomOverWDeg(ArrayUtils.flatten(x), seed), new InDomainMax()));
    // solver.set(new Assignment(new MaxRegret(ArrayUtils.flatten(x)), new InDomainMiddle()));

    // These values are from 
    //    choco-parser/src/main/java/parser/flatzinc/ast/FGoal.java
    // * @param VARS           collection of variables
    // * @param GAMMA          aging parameters
    // * @param DELTA          for interval domain size estimation
    // * @param ALPHA          forget parameter
    // * @param RESTART        restart parameter
    // * @param FORCE_SAMPLING minimal number of iteration for sampling phase
    // * @param SEED           the seed for random
    // solver.set(IntStrategyFactory.ActivityBased(ArrayUtils.flatten(x), solver, 0.999d, 0.2d, 8, 1.1d, 1, seed)); // orig
    // solver.set(IntStrategyFactory.ActivityBased(ArrayUtils.flatten(x), solver, 0.999d, 0.9d, 8, 1.1d, 1, seed)); 
    solver.set(IntStrategyFactory.ActivityBased(ArrayUtils.flatten(x), solver, 0.999d, 0.95d, 8, 1.9d, 1, seed)); 

    // Values from 
    //   choco-samples/src/main/java/samples/integer/MagicSquare.java:
    // ImpactBased(IntVar[] VARS, int ALPHA, int SPLIT, int NODEIMPACT, long SEED, boolean INITONLY)
    // * @param VARS       variables of the problem (should be integers)
    // * @param ALPHA      aging parameter
    // * @param SPLIT      split parameter for subdomains computation
    // * @param NODEIMPACT force update of impacts every nodeImpact nodes. Set value to 0 to avoid using it.
    // * @param SEED       a seed for random
    // * @param INITONLY   only apply the initialisation phase, do not update impact thereafter
    // solver.set(new ImpactBased(ArrayUtils.flatten(x), 2, 3, 10, seed, false)); // original

    // Using Random value selection: Not too bad
    // solver.set(new Assignment(new MaxRegret(ArrayUtils.flatten(x)), new InDomainRandom(seed)));
    // solver.set(new Assignment(new FirstFail(ArrayUtils.flatten(x)), new InDomainRandom(seed)));
    // solver.set(new Assignment(new InputOrder(ArrayUtils.flatten(x)), new InDomainRandom(seed)));
    // solver.set(new Assignment(new DomOverWDeg(ArrayUtils.flatten(x), seed), new InDomainRandom(seed)));


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
        
        System.out.println("n : " + n);
        for(int i = 0; i < n; i++) {
          for(int j = 0; j < n; j++) {
            System.out.print(x[i][j].getValue() + " ");
          }
          System.out.println();
        }
        System.out.println();

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

    new Grid17x17().execute(args);

  }
}

