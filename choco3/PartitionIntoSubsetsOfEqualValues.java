/**
 *
 * Partitioning set into subsets in Choco3
 *
 * From Programmers Stack Exchange (C#)
 * http://programmers.stackexchange.com/questions/153184/partitioning-set-into-subsets-with-respect-to-equality-of-sum-among-subsets
 * Partitioning set into subsets with respect to equality of sum among subsets
 * """
 * let say i have {3, 1, 1, 2, 2, 1,5,2,7} set of numbers, I need to split the 
 * numbers such that sum of subset1 should be equal to sum of subset2 
 * {3,2,7} {1,1,2,1,5,2}. First we should identify whether we can split number(one 
 * way might be dividable by 2 without any remainder) and if we can, we should 
 * write our algorithm two create s1 and s2 out of s.
 *
 * How to proceed with this approach? I read partition problem in wiki and even in some 
 * articles but i am not able to get anything. Can someone help me to find the 
 * right algorithm and its explanation in simple English?
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
import util.tools.StatisticUtils;

import java.util.*;

public class PartitionIntoSubsetsOfEqualValues extends AbstractProblem {

  int[] s = {3, 1, 1, 2, 2, 1, 5, 2, 7};
  int n = s.length;
  int num_subsets = 2;

  
  BoolVar[][] x;
  IntVar[] sums;

  @Override
  public void buildModel() {

    int s_sum = StatisticUtils.sum(s);

    x = VariableFactory.boolMatrix("x", num_subsets, n, solver);
    sums = VariableFactory.boundedArray("sums", num_subsets, 0, s_sum, solver);

    // Ensure that a number is in exact one subset
    for(int k = 0; k < n; k++) {
      BoolVar[] tmp1 = VariableFactory.boolArray("tmp1_"+k, num_subsets, solver);
      for(int p = 0; p < num_subsets; p++) {
        tmp1[p] = x[p][k];
      }
      solver.post(IntConstraintFactory.sum(tmp1, VariableFactory.fixed(1, solver)));

    }

    // Ensure that the sum of all subsets are the same.
    for(int p = 0; p < num_subsets; p++) {
      IntVar[] ptmp = VariableFactory.boundedArray("ptmp_"+p, n, 0, s_sum, solver);
      for(int k = 0; k < n; k++) {
        solver.post(IntConstraintFactory.times(x[p][k], 
                                               VariableFactory.fixed(s[k], solver), 
                                               ptmp[k]));
      }

      solver.post(IntConstraintFactory.sum(ptmp, sums[p]));
    }

    for(int p = 0; p < num_subsets-1; p++) {
      solver.post(IntConstraintFactory.arithm(sums[p], "=", sums[p+1]));
    }

    // symmetry breaking: assign first number to subset 0
    solver.post(IntConstraintFactory.arithm(x[0][0], "=", 0));

  }

  @Override
  public void createSolver() {
    solver = new Solver("PartitionIntoSubsetsOfEqualValues");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(ArrayUtils.flatten(x)));
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
        
        for(int p = 0; p < num_subsets; p++) {
          System.out.format("Subset %d (sum %d): ", p, sums[p].getValue());
          for(int k = 0; k < n; k++) {
            if (x[p][k].getValue() == 1) {
              System.out.print(k + " ");
            }
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

    new PartitionIntoSubsetsOfEqualValues().execute(args);

  }
}

