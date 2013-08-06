/**
 *
 * Post office problem in Choco3
 *
 * Problem statement:
 * http://www-128.ibm.com/developerworks/linux/library/l-glpk2/
 *
 * From Winston 'Operations Research: Applications and Algorithms':
 * """
 * A post office requires a different number of full-time employees working
 * on different days of the week [summarized below]. Union rules state that
 * each full-time employee must work for 5 consecutive days and then receive
 * two days off. For example, an employee who works on Monday to Friday
 * must be off on Saturday and Sunday. The post office wants to meet its
 * daily requirements using only full-time employees. Minimize the number
 * of employees that must be hired.
 *
 * To summarize the important information about the problem:
 *
 * Every full-time worker works for 5 consecutive days and takes 2 days off
 * - Day 1 (Monday): 17 workers needed
 * - Day 2 : 13 workers needed
 * - Day 3 : 15 workers needed
 * - Day 4 : 19 workers needed
 * - Day 5 : 14 workers needed
 * - Day 6 : 16 workers needed
 * - Day 7 (Sunday) : 11 workers needed
 *
 * The post office needs to minimize the number of employees it needs
 * to hire to meet its demand.
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

import java.util.*;

public class PostOfficeProblem extends AbstractProblem {

  // days 0..6, monday 0
  int n = 7;
  int[] need = {17, 13, 15, 19, 14, 16, 11};
  
  // Total cost for the 5 day schedule.
  // Base cost per day is 100.
  // Working saturday is 100 extra
  // Working sunday is 200 extra.
  int[] cost = {500, 600, 800, 800, 800, 800, 700};

  
  IntVar[] x;
  IntVar total_cost;
  IntVar num_workers;

  @Override
  public void buildModel() {

    x = VariableFactory.boundedArray("x", n, 0, 100, solver);
    total_cost = VariableFactory.bounded("total_cost", 0, 20000, solver);
    num_workers = VariableFactory.bounded("num_workers", 0, 200, solver);


    solver.post(IntConstraintFactory.scalar(x, cost, total_cost));
    solver.post(IntConstraintFactory.sum(x, num_workers));

    for(int i = 0; i < n; i++) {
      ArrayList<IntVar> s = new ArrayList<IntVar>();
      for(int j = 0; j < n; j++) {
        if (j != ((i+5) % n) && j != ((i+6) % n)) {
          s.add(x[j]);
        }
      }
      IntVar s2 = VariableFactory.bounded("s2_"+i, 0, 1000, solver);
      solver.post(IntConstraintFactory.sum(s.toArray(new IntVar[1]), s2));
      solver.post(IntConstraintFactory.arithm(s2, ">=", VariableFactory.fixed(need[i], solver)));

    }


  }

  @Override
  public void createSolver() {
    solver = new Solver("PostOfficeProblem");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(x));
  }

  @Override
  public void solve() {
    solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, total_cost);
    // solver.findSolution();

  }

  @Override
  public void prettyOut() {

    if(solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      do {
        
        System.out.println("total_cost: " + total_cost.getValue());
        System.out.println("num_workers: " + num_workers.getValue());
        for(int i = 0; i < n; i++) {
          System.out.print(x[i].getValue() + " ");
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

    new PostOfficeProblem().execute(args);

  }
}

