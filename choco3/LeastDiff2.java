/**
  *
  * Least Diff problem in Choco3.
  *
  * Minimize the difference ABCDE - FGHIJ
  *                     where A..J is all different in the range 0..9.
  *
  * The solution is: 50123 - 49876 = 247
  *
  *
  * Choco3 model by Hakan Kjellerstrand (hakank@gmail.com)
  * Also see http://www.hakank.org/choco3/
  *
  */
import org.kohsuke.args4j.Option;
import org.slf4j.LoggerFactory;
import samples.AbstractProblem;
import solver.ResolutionPolicy;
import solver.Solver;
import solver.constraints.Constraint;
import solver.constraints.IntConstraintFactory;
import solver.constraints.nary.cnf.Literal;
import solver.constraints.nary.cnf.Node;
import solver.constraints.nary.cnf.Node.*;
import solver.constraints.nary.cnf.ALogicTree;
import solver.search.limits.FailLimit;
import solver.search.strategy.IntStrategyFactory;
import solver.search.loop.monitors.SearchMonitorFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import util.ESat;
import util.tools.ArrayUtils;

import java.util.*;

public class LeastDiff2 extends AbstractProblem {

  IntVar[] letters;
  IntVar A,B,C,D,E,F,G,H,I,J;
  IntVar Diff, X, Y;

  @Override
  public void createSolver() {
    solver = new Solver("LeastDiff2");
  }

  @Override
  public void buildModel () {

    A = VariableFactory.enumerated("A", 0, 9, solver);
    B = VariableFactory.enumerated("B", 0, 9, solver);
    C = VariableFactory.enumerated("C", 0, 9, solver);
    D = VariableFactory.enumerated("D", 0, 9, solver);
    E = VariableFactory.enumerated("E", 0, 9, solver);
    F = VariableFactory.enumerated("F", 0, 9, solver);
    G = VariableFactory.enumerated("G", 0, 9, solver);
    H = VariableFactory.enumerated("H", 0, 9, solver);
    I = VariableFactory.enumerated("I", 0, 9, solver);
    J = VariableFactory.enumerated("J", 0, 9, solver);
    letters = new IntVar[] {A,B,C,D,E,F,G,H,I,J};

    Diff = VariableFactory.bounded("Diff", 0, 1000, solver);

    // temporary variables to build Diff
    X = VariableFactory.bounded("X", 0, 100000, solver);
    Y = VariableFactory.bounded("Y", 0, 100000, solver);

    // all unique
    solver.post(IntConstraintFactory.alldifferent(letters, "BC"));

    // X = A+B+C+D+E
    solver.post(IntConstraintFactory.scalar(new IntVar[]{A,B,C,D,E},
                                            new int[]{10000, 1000, 100, 10, 1},
                                            X));
    // Y = F +G + H + I + J
    solver.post(IntConstraintFactory.scalar(new IntVar[]{F,G,H,I,J},
                                            new int[]{10000, 1000, 100, 10, 1}, 
                                            Y));

    // Diff = X - Y
    /*
    solver.post(IntConstraintFactory.arithm(X, ">", Y));
    solver.post(IntConstraintFactory.distance(X, Y, "=", Diff));
    */

    // Alternative 
    solver.post(IntConstraintFactory.sum(new IntVar[] {X,VariableFactory.minus(Y)}, Diff));

  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(letters));
  }

  @Override
  public void solve() {
    solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, Diff);
  }

  @Override
  public void prettyOut() {

    LoggerFactory.getLogger("bench").info("LeastDiff2");
    StringBuilder st = new StringBuilder();

    st.append("\n" + X.getValue() + "-" + Y.getValue() + " = " + Diff.getValue() + "\n");

    LoggerFactory.getLogger("bench").info(st.toString());
    
  } // end puzzle

  public static void main(String[] args) {
    new LeastDiff2().execute(args);
  }



} // end class
