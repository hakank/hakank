/**
 *
 * Map coloring in Choco3.
 *
 * This is a direct version. A more general version is here
 * http://www.hakank.org/choco3/Map2.java
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

public class Map extends AbstractProblem {

  @Option(name = "-solutions", usage = "number of solutions to show (default all, 0).", required = false)
  int solutions = 0;

  int n;

  // Decision variables
  IntVar[] color;


  @Override
  public void buildModel() {

    //
    // data
    //
    int Belgium     = 0;
    int Denmark     = 1;
    int France      = 2;
    int Germany     = 3;
    int Netherlands = 4;
    int Luxembourg  = 5;

    n = 6;
    int max_num_colors = 4;

    color = VariableFactory.enumeratedArray("color", n, 1, max_num_colors, solver);


    solver.post(IntConstraintFactory.arithm(color[France], "!=", color[Belgium]));
    solver.post(IntConstraintFactory.arithm(color[France], "!=", color[Luxembourg]));
    solver.post(IntConstraintFactory.arithm(color[France], "!=", color[Germany]));
    solver.post(IntConstraintFactory.arithm(color[Luxembourg], "!=", color[Germany]));
    solver.post(IntConstraintFactory.arithm(color[Luxembourg], "!=", color[Belgium]));
    solver.post(IntConstraintFactory.arithm(color[Belgium], "!=", color[Netherlands]));
    solver.post(IntConstraintFactory.arithm(color[Belgium], "!=", color[Germany]));
    solver.post(IntConstraintFactory.arithm(color[Germany], "!=", color[Netherlands]));
    solver.post(IntConstraintFactory.arithm(color[Germany], "!=", color[Denmark]));

    // Symmetry breaking
    solver.post(IntConstraintFactory.arithm(color[Belgium], "=", 1));


  }

  @Override
  public void createSolver() {
    solver = new Solver("Map");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(color));
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
          System.out.print(color[i].getValue() + " ");
        }
        System.out.println("\n");

        num_solutions++;
        if (solutions > 0 && num_solutions >= solutions) {
          break;
        }
        
      } while (solver.nextSolution() == Boolean.TRUE);
      
      System.out.println("It was " + num_solutions + " solutions.");
      
    }  else {
      System.out.println("No solution.");
    }
    
  }


  public static void main(String args[]) {

    new Map().execute(args);

  }


}

