/**
 *
 * Map coloring in Choco3.
 *
 * This is a more general version of this problem
 * http://www.hakank.org/choco3/Map.java
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
import solver.search.strategy.selectors.variables.*;
import util.ESat;
import util.tools.ArrayUtils;

public class Map2 extends AbstractProblem {

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

    int[][] neighbours = {{France,     Belgium},
                          {France,     Luxembourg},
                          {France,     Germany},
                          {Luxembourg, Germany},
                          {Luxembourg, Belgium},
                          {Belgium,    Netherlands},
                          {Belgium,    Germany},
                          {Germany,    Netherlands},
                          {Germany,    Denmark}};

    color = VariableFactory.enumeratedArray("color", n, 1, max_num_colors, solver);

    for(int i = 0; i < neighbours.length; i++) {
      solver.post(IntConstraintFactory.arithm(color[neighbours[i][0]], 
                                              "!=", 
                                              color[neighbours[i][1]]));
    }

    // Symmetry breaking
    solver.post(IntConstraintFactory.arithm(color[Belgium], "=", 1));


  }

  @Override
  public void createSolver() {
    solver = new Solver("Map2");
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

    new Map2().execute(args);

  }


}

