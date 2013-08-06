/**
 *
 * Set covering problem in Choco3.
 *
 *  This example is from the OPL example covering.mod
 *  """
 *  Consider selecting workers to build a house. The construction of a 
 *  house can be divided into a number of tasks, each requiring a number of 
 *  skills (e.g., plumbing or masonry). A worker may or may not perform a 
 *  task, depending on skills. In addition, each worker can be hired for a 
 *  cost that also depends on his qualifications. The problem consists of 
 *  selecting a set of workers to perform all the tasks, while minimizing the 
 *  cost. This is known as a set-covering problem. The key idea in modeling 
 *  a set-covering problem as an integer program is to associate a 0/1 
 *  variable with each worker to represent whether the worker is hired. 
 *  To make sure that all the tasks are performed, it is sufficient to 
 *  choose at least one worker by task. This constraint can be expressed by a 
 *  simple linear inequality.
 *  """
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
import util.ESat;
import util.tools.ArrayUtils;

public class CoveringOpl extends AbstractProblem {

  int num_workers;
  int num_tasks;
  int[][] qualified;
  int[] cost;

  IntVar[] hire;
  IntVar total_cost;

  @Override
  public void buildModel() {

    num_workers = 32;
    num_tasks = 15;

    // Which worker is qualified for each task.
    // Note: This is 1-based and will be made 0-base below.
    qualified = new int[][] {{ 1,  9, 19,  22,  25,  28,  31 },
                                     { 2, 12, 15, 19, 21, 23, 27, 29, 30, 31, 32 },
                                     { 3, 10, 19, 24, 26, 30, 32 },
                                     { 4, 21, 25, 28, 32 },
                                     { 5, 11, 16, 22, 23, 27, 31 },
                                     { 6, 20, 24, 26, 30, 32 },
                                     { 7, 12, 17, 25, 30, 31 } ,
                                     { 8, 17, 20, 22, 23  },
                                     { 9, 13, 14,  26, 29, 30, 31 },
                                     { 10, 21, 25, 31, 32 },
                                     { 14, 15, 18, 23, 24, 27, 30, 32 },
                                     { 18, 19, 22, 24, 26, 29, 31 },
                                     { 11, 20, 25, 28, 30, 32 },
                                     { 16, 19, 23, 31 },
                                     { 9, 18, 26, 28, 31, 32 }};
    cost = new int[] {1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3,
                      3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 8, 9};

    hire = VariableFactory.enumeratedArray("hire", num_workers, 0, 1, solver);
    total_cost = VariableFactory.bounded("total_cost", 0, 100, solver);

    for(int j = 0; j < num_tasks; j++) {
      // Sum the cost for hiring the qualified workers
      // (also, make 0-base).
      int len = qualified[j].length;
      IntVar[] tmp = new IntVar[len];
      for(int c = 0; c < len; c++) {
        tmp[c] = hire[qualified[j][c] - 1];
      }

      IntVar s = VariableFactory.bounded("s", 0, len, solver);
      solver.post(IntConstraintFactory.sum(tmp, s));
      solver.post(IntConstraintFactory.arithm(s,">=", 1));

    }

    solver.post(IntConstraintFactory.scalar(hire, cost, total_cost));

  }

  @Override
  public void createSolver() {
    solver = new Solver("CoveringOpl");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(hire));
  }

  @Override
  public void solve() {
    solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, total_cost);
  }


  @Override
  public void prettyOut() {
 
    if (solver.isFeasible() == ESat.TRUE) {
      do {

        System.out.println("Cost: " + total_cost.getValue());
        System.out.print("Hire: ");
        for(int i = 0; i < num_workers; i++) {
          if (hire[i].getValue() == 1) {
            System.out.print(i + " ");
          }
        }
        System.out.println("\n");
        
      } while (solver.nextSolution() == Boolean.TRUE);


    }  else {
      System.out.println("No solution.");
    }

  }


  public static void main(String args[]) {

    new CoveringOpl().execute(args);

  }


}

