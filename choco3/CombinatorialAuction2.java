/**
 *
 * Combinatorial auction in Choco3.
 *
 * http://en.wikipedia.org/wiki/Combinatorial_auction
 * """
 * A combinatorial auction is an auction in which bidders can place 
 * bids on combinations of items, or "packages," rather than 
 * just individual items. Simple combinatorial auctions have been 
 * used for many years in estate auctions, where a common procedure 
 * is to auction the individual items and then at the end to accept 
 * bids for packages of items.
 * """
 *
 * This simple example is from the lecture slides
 * Constraint Satisfaction Problems, Constraint Optimization
 * by Bernhard Nebel and Stefan Wölfl
 * http://www.informatik.uni-freiburg.de/~ki/teaching/ws0910/csp/csp10-handout4.pdf
 * """
 * In combinatorial auctions, bidders can give bids for set of items.
 * The auctioneer [then] has to generate an optimial selection, e.g.
 * one that maximizes revenue.
 * 
 * Definition
 * The combinatorial auction problem  is specified as follows:
 *   Given: A set of items Q = {q1,...,qn} and a set of bids
 *          B = {b1,...,bm} such that each bid is bi = (Qi, ri),
 *          where Qi (= Q and ri is a strictly positive real number.
 *   Task: Find a subset of bids B'(= B such that any two bids in B'
 *         do not share an item maximizing Sum(Qi,ri) (= Biri.
 *
 * """ 

 * This is a more general model for the combinatorial example
 * in the Numberjack Tutorial, pages 9 and 24 (slides  19/175 and
 * 51/175).
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

public class CombinatorialAuction2 extends AbstractProblem {

  int n = 5;
  
  // the items for each bid
  int[][] items = {
    new int[] {0,1},   // A,B
    new int[] {0,2},   // A, C
    new int[] {1,3},   // B,D
    new int[] {1,2,3}, // B,C,D
    new int[] {0}      // A
  };
  
  int[] bid_ids = {0,1,2,3};
  int[] bid_amount = {10,20,30,40,14};
  

  IntVar[] x;
  IntVar z;


  @Override
  public void buildModel() {
    
    x = VariableFactory.enumeratedArray("x", n, 0, 1, solver);
    z = VariableFactory.enumerated("z", 0, 100, solver);

    for(int bid_id : bid_ids) {
      ArrayList<IntVar> tmp = new ArrayList<IntVar>();
      for (int item = 0; item < n; item++) {
        for(int i = 0; i < items[item].length; i++) {
          if (items[item][i] == bid_id) {
            tmp.add(x[item]);
          }
        }
      }

      IntVar tmpsum = VariableFactory.bounded("tmpsum", 0, 100, solver);
      solver.post(IntConstraintFactory.sum(tmp.toArray(new IntVar[1]), tmpsum));
      solver.post(IntConstraintFactory.arithm(tmpsum, "<=", 1));


    }


    solver.post(IntConstraintFactory.scalar(x, bid_amount, z));

  }




  @Override
  public void createSolver() {
    solver = new Solver("CombinatorialAuction2");
  }

  @Override
  public void configureSearch() {
    // solver.set(IntStrategyFactory.domOverWDeg_InDomainMin(x, seed));
    solver.set(IntStrategyFactory.firstFail_InDomainMin(x));
  }

  @Override
  public void solve() {
    // solver.findSolution();
    solver.findOptimalSolution(ResolutionPolicy.MAXIMIZE, z);
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      do {
        System.out.println("z: " + z.getValue());
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

    new CombinatorialAuction2().execute(args);

  }

}

