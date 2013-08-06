/**
 *
 * Appointment scheduling (set approach) in Choco3.
 *
 * From Stack Overflow 
 * "Appointment scheduling algorithm (N people with N free-busy slots, constraint-satisfaction)"
 * http://stackoverflow.com/questions/11143439/appointment-scheduling-algorithm-n-people-with-n-free-busy-slots-constraint-sa
 * """
 * Problem statement
 *
 * We have one employer that wants to interview N people, and therefore makes N 
 * interview slots. Every person has a free-busy schedule for those slots. Give an 
 * algorithm that schedules the N people into N slots if possible, and return a 
 * flag / error / etc if it is impossible. What is the fastest possible runtime complexity?
 *
 * My approaches so far
 *
 * Naive: there are N! ways to schedule N people. Go through all of them, for each 
 * permutation, check if it's feasible. O( n! )
 *
 * Backtracking:
 *
 * 1. Look for any interview time slots that can only have 1 person. Schedule the person, 
 *    remove them from the list of candidates and remove the slot.
 * 2. Look for any candidates that can only go into 1 slot. Schedule the person, remove 
 *    them from the list of candidates and remove the slot.
 * 3. Repeat 1 & 2 until there are no more combinations like that.
 * 4. Pick a person, schedule them randomly into one of their available slots. Remember 
 *    this operation.
 * 5. Repeat 1, 2, 3 until we have a schedule or there is an unresolvable conflict. If we 
 *    have a schedule, return it. If there's an unresolvable conflict, backtrack.
 *
 * This is O( n! ) worst case, I think - which isn't any better.
 *
 * There might be a D.P. solution as well - but I'm not seeing it yet.
 *
 * Other thoughts
 *
 * The problem can be represented in an NxN matrix, where the rows are "slots", columns 
 * are "people", and the values are "1" for free and "0" for busy. Then, we're looking for 
 * a row-column-swapped Identity Matrix within this matrix. Steps 1 & 2 are looking for 
 * a row or a column with only one "1". (If the rank of the matrix is = N, I that means that 
 * there is a solution. But the opposite does not hold) Another way to look at it is to 
 * treat the matrix as an unweighed directed graph edge matrix. Then, the nodes each 
 * represent 1 candidate and 1 slot. We're then looking for a set of edges so that every 
 * node in the graph has one outgoing edge and one incoming edge. Or, with 2x nodes, it would 
 * be a bipartite graph.
 *
 * Example of a matrix:
 *
 *   1 1 1 1
 *   0 1 1 0
 *   1 0 0 1
 *   1 0 0 1
 * """
 *
 * This model implements the set based approach mentioned in my answer to the
 * question.
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
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.explanations.ExplanationFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import solver.search.strategy.selectors.variables.*;
import util.ESat;
import util.tools.ArrayUtils;

import java.util.*;

public class AppointmentScheduling extends AbstractProblem {


  // the original problem cited above
  int[][] s = new int[][] {
    new int[] {1,2,3,4},
    new int[] {2,3},
    new int[] {1,4},
    new int[] {1,4}
  };
  
  int n;

  IntVar[] x;


  @Override
  public void buildModel() {
  
    n = s.length;

    // The assignment of persons to a time slot (appointment number 1..n).
    x = VariableFactory.enumeratedArray("x", n, 1, n, solver);


    // Ensure that each person is alotted to exactly one time slot only.
    solver.post(IntConstraintFactory.alldifferent(x, "BC"));

    // Ensure that the selected person for the alotted time is avaiable.
    BoolVar[] b = VariableFactory.boolArray("b", n, solver);
    for(int i = 0; i < n; i++) {
      solver.post(IntConstraintFactory.implies(b[i],
                                               IntConstraintFactory.member(x[i], s[i])));
      solver.post(IntConstraintFactory.implies(VariableFactory.not(b[i]),
                                                                   IntConstraintFactory.not_member(x[i], s[i])));
    }
    solver.post(IntConstraintFactory.sum(b, VariableFactory.fixed(n, solver)));
  
  }


  @Override
  public void createSolver() {
    solver = new Solver("AppointmentScheduling");
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
        System.out.print("x: ");
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

    new AppointmentScheduling().execute(args);

  }

}

