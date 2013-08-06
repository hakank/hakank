/**
 *
 * SEND+MORE=MONEY in Choco3.
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

public class SendMoreMoney extends AbstractProblem {

  String[] x_s = {"s","e","n","d","m","o","r","y"};

  IntVar[] x;


  @Override
  public void buildModel() {

    IntVar s = VariableFactory.enumerated("s", 0, 9, solver);
    IntVar e = VariableFactory.enumerated("e", 0, 9, solver);
    IntVar n = VariableFactory.enumerated("n", 0, 9, solver);
    IntVar d = VariableFactory.enumerated("d", 0, 9, solver);
    IntVar m = VariableFactory.enumerated("m", 0, 9, solver);
    IntVar o = VariableFactory.enumerated("o", 0, 9, solver);
    IntVar r = VariableFactory.enumerated("r", 0, 9, solver);
    IntVar y = VariableFactory.enumerated("y", 0, 9, solver);

    x = new IntVar[] {s, e, n, d, m, o, r, y};

    IntVar send = VariableFactory.bounded("send", 1000, 9999, solver);
    IntVar more = VariableFactory.bounded("more", 1000, 9999, solver);
    IntVar money = VariableFactory.bounded("money", 10000, 99999, solver);

    solver.post(IntConstraintFactory.scalar(new IntVar[]{s,e,n,d},
                                            new int[]{1000, 100, 10, 1},
                                            send));

    solver.post(IntConstraintFactory.scalar(new IntVar[]{m,o,r,e},
                                            new int[]{1000, 100, 10, 1},
                                            more));
    solver.post(IntConstraintFactory.scalar(new IntVar[]{m,o,n,e,y},
                                            new int[]{10000, 1000, 100, 10, 1},
                                            money));

    solver.post(IntConstraintFactory.sum(new IntVar[]{send, more}, money));

    
    solver.post(IntConstraintFactory.alldifferent(x, "BC"));

    solver.post(IntConstraintFactory.arithm(s,">", 0));
    solver.post(IntConstraintFactory.arithm(m,">", 0));
    

  }

  @Override
  public void createSolver() {
    solver = new Solver("SendMoreMoney");
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
        for(int i = 0; i < x.length; i++) {
          System.out.println(x_s[i] + ": " + x[i].getValue());
        }
        System.out.println("\n");

        num_solutions++;

      } while (solver.nextSolution() == Boolean.TRUE);
      
      System.out.println("It was " + num_solutions + " solutions.");
      
    }  else {
      System.out.println("No solution.");
    }
    
  }


  public static void main(String args[]) {

    new SendMoreMoney().execute(args);

  }


}

