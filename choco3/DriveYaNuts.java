/**
 *
 * Drive Ya Crazy problem in Choco3
 *
 * From http://www.samstoybox.com/toys/DriveYaNuts.html
 * """
 * The Drive Ya Nuts puzzle by Milton Bradley was cool and quite difficult. The object of 
 * the puzzle is to place all seven nuts such that the numbers on all sides of each 
 * nut match the numbers on the adjoining nut. There is but one way to solve the puzzle. 
 * Here are two versions of puzzle. Note that the second one is still factory sealed and 
 * shows the solution. So you think it sounds easy? 
 * """
 *
 * Some other links: 
 *   http://www.jaapsch.net/puzzles/circus.htm
 *
 *
 * Representation:
 *
 * A side of a nut is numbered as following
 * 
 *            1
 *
 *       6        2
 *   
 *       5        3
 *
 *            4
 *
 *
 * and the 7 nuts are numbered as follows:
 *
 *            1 
 *
 *        6       2
 *            7
 *        5        3
 * 
 *            4
 * 
 * i.e. nut 7 is the master (center) nut.
 *
 *
 * Note: There are 6 solutions, depending on how we orient
 *       the center nut (7). This is handled by symmetry breaking below.
 *
 * Here is one solution (which has the center nut start with 1):
 * 
 *    2 3 5 1 4 6   * Nut 1 (in the representation above)
 *    3 2 4 1 6 5   * Nut 2
 *    1 4 3 6 5 2   * Nut 3
 *    4 5 6 1 2 3   * Nut 4
 *    2 5 3 1 6 4   * Nut 5
 *    5 4 3 2 1 6   * Nut 6
 *    1 6 2 4 5 3   * Nut 7 (center nut)
 *
 * E.g. the first nut is the nut 1,4,6,2,3,5 rotated like this, i.e.
 * with 2 at 12 o'clock and then clock wise: 2,3,5,1,4, and 6:
 *    
 *            2
 *
 *       6        3
 *   
 *       4        5
 *
 *            1
 *
 * And so on with the other nuts.
 * 
 * 
 * [Personal comment: This is the same puzzle as the infamous 
 *                    AWA-Patent problem from a long long time ago, 
 *                    though I didn't then know what it was called.
 *                    Yes, I did solve it manually without any help.]
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


public class DriveYaNuts extends AbstractProblem {

  @Option(name = "-no_symmetry", usage = "Use no symmetry breaking (default false).", required = false)
  boolean no_symmetry = false;

  // Connections between the nuts, i.e. where the values must 
  // be the same.
  int[][] connections = {
    // nuts    sides to be equal
    {1,2,       3,6},
    {2,3,       4,1},
    {3,4,       5,2},
    {4,5,       6,3},
    {5,6,       1,4},
    {6,1,       2,5},
    
    {7,1,       1,4},
    {7,2,       2,5},
    {7,3,       3,6},
    {7,4,       4,1},
    {7,5,       5,2},
    {7,6,       6,3}};

  int num_connections = connections.length;

  // Nuts

  // This is the nuts in the solution order.
  // 1,4,6,2,3,5, 1,4,6,2,3,5, // 1 
  // 1,6,5,3,2,4, 1,6,5,3,2,4, // 2 
  // 1,4,3,6,5,2, 1,4,3,6,5,2, // 3
  // 1,2,3,4,5,6, 1,2,3,4,5,6, // 4
  // 1,6,4,2,5,3, 1,6,4,2,5,3, // 5
  // 1,6,5,4,3,2, 1,6,5,4,3,2, // 6 
  // 1,6,2,4,5,3, 1,6,2,4,5,3, // 7 // center nut
  
  int[][] nuts = {
    {1,2,3,4,5,6, 1,2,3,4,5,6},  // 4 (row 4 in the solution order shown above)
    {1,4,3,6,5,2, 1,4,3,6,5,2},  // 3
    {1,4,6,2,3,5, 1,4,6,2,3,5},  // 1 
    {1,6,2,4,5,3, 1,6,2,4,5,3},  // 7 [center nut]
    {1,6,4,2,5,3, 1,6,4,2,5,3},  // 5
    {1,6,5,3,2,4, 1,6,5,3,2,4},  // 2 
    {1,6,5,4,3,2, 1,6,5,4,3,2}}; // 6 

  int n = 6; // sides on a nut
  int m = 7; // number of nuts

  IntVar[][] x;
  IntVar[] pos; // positions of the nuts
  IntVar[] pos_inv; // inverted positions (for display and pondering)
  IntVar[] start_ix; // where does nut i start in nuts array

  @Override
  public void buildModel() {

    int[] nuts_flat = ArrayUtils.flatten(nuts);

    x = VariableFactory.enumeratedMatrix("x", m, n, 1, n, solver);
    pos = VariableFactory.enumeratedArray("pos", m, 0, m-1, solver);
    pos_inv = VariableFactory.enumeratedArray("pos_inv", m, 0, m-1, solver);
    start_ix = VariableFactory.enumeratedArray("start_ix", m, 0, n-1, solver);


    for(int i = 0; i < m; i++) {
      solver.post(IntConstraintFactory.alldifferent(x[i], "BC"));

      IntVar k = VariableFactory.enumerated("k_"+i, 0, n-1, solver);
      IntVar p = VariableFactory.enumerated("p_"+i, 0, m-1, solver);

      solver.post(IntConstraintFactory.arithm(start_ix[i],"=", k));
      solver.post(IntConstraintFactory.arithm(pos[i],"=", p));

      for(int j = 0; j < n; j++) {
        // x[i,j] = nuts[p,k+j]
        IntVar pn = VariableFactory.bounded("pn", 0, m*2*n, solver);
        solver.post(IntConstraintFactory.times(p, VariableFactory.fixed(n*2, solver), pn));
        IntVar pnkj = VariableFactory.bounded("pnkj", 0, 2*n*m, solver);
        solver.post(IntConstraintFactory.sum(new IntVar[]{pn, VariableFactory.offset(k, j)}, pnkj));
        solver.post(IntConstraintFactory.element(x[i][j], nuts_flat, pnkj, 0, "detect"));
      }

    }

    solver.post(IntConstraintFactory.alldifferent(pos, "BC"));
    // solver.post(IntConstraintFactory.alldifferent(pos_inv, "BC"));
    solver.post(IntConstraintFactory.inverse_channeling(pos, pos_inv, 0,0));


    for(int c = 0; c < num_connections; c++) {
      solver.post(IntConstraintFactory.arithm(
                                              x[connections[c][0]-1][connections[c][2]-1],
                                              "=",
                                              x[connections[c][1]-1][connections[c][3]-1]));
    }


    if (!no_symmetry) {
      solver.post(IntConstraintFactory.arithm(start_ix[m-1], "=", VariableFactory.fixed(0, solver)));
    }


  }

  @Override
  public void createSolver() {
    solver = new Solver("DriveYaNuts");
  }

  @Override
  public void configureSearch() {
    // solver.set(IntStrategyFactory.domOverWDeg_InDomainMin(ArrayUtils.append(pos, ArrayUtils.flatten(x)), seed));
    solver.set(IntStrategyFactory.maxReg_InDomainMin(ArrayUtils.append(pos, ArrayUtils.flatten(x))));
  }

  @Override
  public void solve() {
    solver.findSolution();

  }

  @Override
  public void prettyOut() {

    if(solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      do {

        System.out.print("pos (1-based)    : ");
        for(int i = 0; i < m; i++) {
          System.out.print(pos[i].getValue()+1 + " ");
        }
        System.out.println();
        System.out.print("pos_inv (1-based): ");
        for(int i = 0; i < m; i++) {
          System.out.print(pos_inv[i].getValue()+1 + " ");
        }
        System.out.println();
        System.out.print("start_ix         : ");
        for(int i = 0; i < m; i++) {
          System.out.print(start_ix[i].getValue() + " ");
        }
        System.out.println();
        
        System.out.println("x:");
        for(int i = 0; i < m; i++) {
          for(int j = 0; j < n; j++) {
            System.out.print(x[i][j].getValue() + " ");
          }
          if (i == m-1) {
            System.out.print(" [center]");
          }
          System.out.println();
        }
        System.out.println();


        num_solutions++;

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

    new DriveYaNuts().execute(args);

  }
}

