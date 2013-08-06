/**
 *
 * Scheduling speakers in Choco3.
 *
 * From Rina Dechter, Constraint Processing, page 72
 * Scheduling of 6 speakers in 6 slots.
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

public class SchedulingSpeakers extends AbstractProblem {

  int n = 6; // number of speakers

  // slots available to speak
  int[][] available = {
                            // Reasoning:
    new int[] {3,4,5,6},    // 2) the only one with 6 after speaker F -> 1
    new int[] {3,4},        // 5) 3 or 4
    new int[] {2,3,4,5},    // 3) only with 5 after F -> 1 and A -> 6
    new int[] {2,3,4},      // 4) only with 2 after C -> 5 and F -> 1
    new int[] {3,4},        // 5) 3 or 4
    new int[] {1,2,3,4,5,6} // 1) the only with 1
  };
  
  IntVar[] x;


  @Override
  public void buildModel() {

    x = VariableFactory.enumeratedArray("x", n, 1, n, solver);


    solver.post(IntConstraintFactory.alldifferent(x, "BC"));

    
    for(int i = 0; i < n; i++) {
      solver.post(IntConstraintFactory.member(x[i], available[i]));
    }

  }

  @Override
  public void createSolver() {
    solver = new Solver("SchedulingSpeakers");
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
        System.out.println("x: ");
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

    new SchedulingSpeakers().execute(args);

  }


}

