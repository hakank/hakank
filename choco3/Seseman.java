/*
 * The Seseman's Convent Problem in Choco3.
 *
 * Given the following matrix:
 *
 *    A  B  C
 *    D  _  E
 *    F  G  H
 *
 * calculate the following constraints:
 *
 *   A + B + C = RowSum
 *   A + D + F = RowSum
 *   C + E + H = RowSum
 *   F + G + H = RowSum
 *   A + B + C + D + E + F + G + H = Total
 *
 *
 * For more information about this problem:
 * - Swedish blog post with a fuller description of the problem:
 *   "Sesemans matematiska klosterproblem samt lite Constraint Logic Programming"
 *   http://www.hakank.org/webblogg/archives/001084.html
 *
 * A CGI-program which uses the ECLiPSe program mentioned above
 *   http://www.hakank.org/seseman/seseman.cgi
 *
 * Choco model by Hakan Kjellerstrand (hakank@gmail.com)
 * Also see http://www.hakank.org/choco3/
 *
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
import solver.search.limits.FailLimit;
import solver.search.strategy.IntStrategyFactory;
import solver.search.loop.monitors.SearchMonitorFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import util.ESat;
import util.tools.ArrayUtils;


public class Seseman extends AbstractProblem {

  @Option(name = "-start", usage = "start (default 1, 0=allow empty rooms).", required = false)
  int start = 1;
  @Option(name = "-total", usage = "total (default 24).", required = false)
  int total = 24;
  @Option(name = "-row_sum", usage = "total (default 9).", required = false)
  int row_sum = 9;

  IntVar A,B,C,D,E,F,G,H;
  IntVar Total, RowSum;
  IntVar[] letters;
  

  @Override
  public void buildModel() {

    // 0..9: allow empty rooms 1..9: don't allow empty rooms
    A = VariableFactory.enumerated("A", start, 9, solver);
    B = VariableFactory.enumerated("B", start, 9, solver);
    C = VariableFactory.enumerated("C", start, 9, solver);
    D = VariableFactory.enumerated("D", start, 9, solver);
    E = VariableFactory.enumerated("E", start, 9, solver);
    F = VariableFactory.enumerated("F", start, 9, solver);
    G = VariableFactory.enumerated("G", start, 9, solver);
    H = VariableFactory.enumerated("H", start, 9, solver);
    letters = new IntVar[] {A,B,C,D,E,F,G,H};

    Total = VariableFactory.fixed(total, solver);
    RowSum = VariableFactory.fixed(row_sum, solver);


    solver.post(IntConstraintFactory.sum(new IntVar[]{A, B, C}, RowSum));
    solver.post(IntConstraintFactory.sum(new IntVar[]{A, D, F}, RowSum));
    solver.post(IntConstraintFactory.sum(new IntVar[]{C, E, H}, RowSum));
    solver.post(IntConstraintFactory.sum(new IntVar[]{F, G, H}, RowSum));

    solver.post(IntConstraintFactory.sum(letters, Total));


  }


  @Override
  public void createSolver() {
    solver = new Solver("Seseman");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(letters));
  }

  @Override
  public void solve() {
    solver.findSolution();
  }

  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_sols = 0;
      do {
        num_sols++;
        System.out.println("\nSolution #" + num_sols);
        System.out.println("" + A.getValue() + " " + B.getValue() + " " + C.getValue());
        System.out.println("" + D.getValue() + " _ " + D.getValue());
        System.out.println("" + E.getValue() + " " + F.getValue() + " " + G.getValue());
        System.out.println("Total: " + Total.getValue() + " RowSum: " + RowSum.getValue());
        System.out.println();
      } while (solver.nextSolution() == Boolean.TRUE);

      System.out.println("There are " + num_sols + " solutions.");

    } else {
      System.out.println("There is no solution.");
    }

  }

  public static void main(String[] args) {
    new Seseman().execute(args);
  }
    


}
