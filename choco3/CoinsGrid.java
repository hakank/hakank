/**
 *
 * Coin Grid problem in Choco3.
 *
 * Problem from 

 * Tony Hürlimann: "A coin puzzle - SVOR-contest 2007"
 * http://www.svor.ch/competitions/competition2007/AsroContestSolution.pdf
 *
 * """
 * In a quadratic grid (or a larger chessboard) with 31x31 cells, one should place coins in such a
 * way that the following conditions are fulfilled:
 *    1. In each row exactly 14 coins must be placed.
 *    2. In each column exactly 14 coins must be placed.
 *    3. The sum of the quadratic horizontal distance from the main diagonal of all cells
 *       containing a coin must be as small as possible.
 *    4. In each cell at most one coin can be placed.
 * The description says to place 14x31 = 434 coins on the chessboard each row containing 14
 * coins and each column also containing 14 coins.
 * """
 *
 * Cf the LPL model:
 * http://diuflx71.unifr.ch/lpl/GetModel?name=/puzzles/coin
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
import solver.search.strategy.IntStrategyFactory;
import solver.search.loop.monitors.SearchMonitorFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import util.ESat;
import util.tools.ArrayUtils;

import java.util.*;

public class CoinsGrid extends AbstractProblem {

  @Option(name = "-n", usage = "n (default 8, original 31)", required = false)
  int n = 8; // original: 31
  @Option(name = "-c", usage = "c (default 3, original 14)", required = false)
  int c = 3; // original: 14

  IntVar[][] x;
  IntVar z;

  @Override
  public void createSolver() {
    solver = new Solver("Diet");
  }


  @Override
  public void buildModel() {

    // Constant
    IntVar v_c = VariableFactory.fixed(c, solver);

    x = VariableFactory.boolMatrix("x", n, n, solver);
        
    for(int i = 0; i < n; i++) {
      ArrayList<IntVar> col = new ArrayList<IntVar>();
      for(int j = 0; j < n; j++) {
        solver.post(IntConstraintFactory.sum(x[i], v_c));
        col.add(x[j][i]);
      }
      solver.post(IntConstraintFactory.sum(col.toArray(new IntVar[1]), v_c));
    }


    /* 
     * to minimize: quadratic horizonal distance, i.e.
     *    sum over x[i][j]*(abs(i-j))*(abs(i-j)) 
     *
     * z should be 13668 for n = 31 and c = 14
     *
     */
    z = VariableFactory.bounded("z", 0, 10000, solver);
    IntVar[] z_array = new IntVar[n*n];
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        IntVar abs_i_j = VariableFactory.fixed(Math.abs(i-j)*Math.abs(i-j), solver);
        z_array[i*n + j] = VariableFactory.bounded("z_" + i +"_" + j, 0, 10000, solver);
        solver.post(IntConstraintFactory.times(x[i][j], abs_i_j, z_array[i*n + j]));
      }
    }
    solver.post(IntConstraintFactory.sum(z_array, z));

  }


  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.domOverWDeg_InDomainMin(ArrayUtils.flatten(x), seed));
    // SearchMonitorFactory.luby(solver, 2, 2, new FailLimit(solver, 2), 2500);
  }

  @Override
  public void solve() {
    solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, z);
  }

  @Override 
  public void prettyOut() {
    LoggerFactory.getLogger("bench").info("CoinsGrid(" + n + "," + c +")\n");
    StringBuilder st = new StringBuilder();

    st.append("z: " + z.getValue() + "\n");
    
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        st.append(x[i][j].getValue()+ " ");
      }
      st.append("\n");
    }

    LoggerFactory.getLogger("bench").info(st.toString());

  }


  public static void main(String args[]) {

    new CoinsGrid().execute(args);

  }
}

