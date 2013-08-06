/**
 *
 * Marathon puzzle in Choco3.
 *
 * From Xpress example
 * http://www.dashoptimization.com/home/cgi-bin/example.pl?id=mosel_puzzle_5_3
 * """
 * Dominique, Ignace, Naren, Olivier, Philippe, and Pascal
 * have arrived as the first six at the Paris marathon.
 * Reconstruct their arrival order from the following
 * information:
 * a) Olivier has not arrived last
 * b) Dominique, Pascal and Ignace have arrived before Naren
 *    and Olivier
 * c) Dominique who was third last year has improved this year.
 * d) Philippe is among the first four.
 * e) Ignace has arrived neither in second nor third position.
 * f) Pascal has beaten Naren by three positions.
 * g) Neither Ignace nor Dominique are on the fourth position.
 *
 * (c) 2002 Dash Associates
 * author: S. Heipcke, Mar. 2002
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

public class Marathon2 extends AbstractProblem {

  int n = 6;
  String[] runners_str = {"Dominique", "Ignace", "Naren",
                          "Olivier", "Philippe", "Pascal"};

  
  IntVar[] runners;
  IntVar[] positions;


  @Override
  public void buildModel() {

    runners = VariableFactory.enumeratedArray("runners", n, 1, n, solver);
    IntVar Dominique = runners[0];
    IntVar Ignace    = runners[1];
    IntVar Naren     = runners[2];
    IntVar Olivier   = runners[3];
    IntVar Philippe  = runners[4];
    IntVar Pascal    = runners[5];

    positions = VariableFactory.enumeratedArray("positions", n, 1, n, solver);

    solver.post(IntConstraintFactory.alldifferent(runners, "BC"));

   // a: Olivier not last
    solver.post(IntConstraintFactory.arithm(Olivier,"!=",n));

    // b: Dominique, Pascal and Ignace before Naren and Olivier
    solver.post(IntConstraintFactory.arithm(Dominique, "<", Naren));
    solver.post(IntConstraintFactory.arithm(Dominique, "<", Olivier));
    solver.post(IntConstraintFactory.arithm(Pascal, "<", Naren));
    solver.post(IntConstraintFactory.arithm(Pascal, "<", Olivier));
    solver.post(IntConstraintFactory.arithm(Ignace, "<", Naren));
    solver.post(IntConstraintFactory.arithm(Ignace, "<", Olivier));

    // c: Dominique better than third
    solver.post(IntConstraintFactory.arithm(Dominique, "<", 3));

    // d: Philippe is among the first four
    solver.post(IntConstraintFactory.arithm(Philippe, "<=", 4));

    // e: Ignace neither second nor third
    solver.post(IntConstraintFactory.arithm(Ignace, "!=", 2));
    solver.post(IntConstraintFactory.arithm(Ignace, "!=", 3));

    // f: Pascal three places earlier than Naren
    solver.post(IntConstraintFactory.arithm(VariableFactory.offset(Pascal,3), "=", Naren));

    // g: Neither Ignace nor Dominique on fourth position
    solver.post(IntConstraintFactory.arithm(Ignace, "!=", 4));
    solver.post(IntConstraintFactory.arithm(Dominique, "!=", 4));

    // channel runners to their positions
    solver.post(IntConstraintFactory.inverse_channeling(runners, positions, 1, 1));

  }

  @Override
  public void createSolver() {
    solver = new Solver("Marathon2");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(runners));
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
        System.out.print("runners  : ");
        for(int i = 0; i < n; i++) {
          System.out.print(runners[i].getValue() + " ");
        }
        System.out.println();
        System.out.print("positions: ");
        for(int i = 0; i < n; i++) {
          System.out.print(positions[i].getValue() + " ");
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

    new Marathon2().execute(args);

  }


}

