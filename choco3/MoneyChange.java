/**
 *
 * Money change in Choco3.
 *
 * How many ways are there to make a change for a dollar..
 *
 * Inspired by
 * Erwin Kalvelagen's 
 * "There are 293 ways to make change for a dollar"
 * http://yetanothermathprogrammingconsultant.blogspot.se/2013/03/there-are-293-ways-to-make-change-for.html
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
import solver.constraints.IntConstraintFactory.*;
import solver.search.strategy.IntStrategyFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import util.ESat;
import util.tools.ArrayUtils;

import java.util.*;

public class MoneyChange extends AbstractProblem {

  @Option(name = "-v", usage = "Change value (default 100).", required = false)
  int v = 100;

  @Option(name = "-denom", usage = "Denominations as a ,-separated string (e.g. \"1,2,5,10,25,50,100\").", required = false)
  String denom = "";


  // In cents (1 dollar = 100 cents)
  int[] denominations = {1,5,10,25,50,100};
  int n = denominations.length;

  IntVar[] x;


  @Override
  public void buildModel() {

    // user defined denominations
    if (denom.length() > 0) {
      String[] s = denom.split("[^\\d]+");
      n = s.length;
      int[] d = new int[n];
      for(int i = 0; i < n; i++) {
        d[i] = Integer.parseInt(s[i]);
      }
      denominations = d;
    }

    x = VariableFactory.enumeratedArray("x", n, 0, v, solver);

    solver.post(IntConstraintFactory.scalar(x, denominations, VariableFactory.fixed(v, solver)));

  }

  @Override
  public void createSolver() {
    solver = new Solver("MoneyChange");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.inputOrder_InDomainMin(x));
  }

  @Override
  public void solve() {
    solver.findSolution();
  }


  @Override
  public void prettyOut() {
 
    if (solver.isFeasible() == ESat.TRUE) {

      int num_solutions = 0;
      ArrayList<ArrayList<Integer>> all = new ArrayList<ArrayList<Integer>>();
      do {

        ArrayList<Integer> res = new ArrayList<Integer>();
        for(int i = 0; i < n; i++) {
          int r = x[i].getValue();
          res.add(r);
          System.out.print(r + " ");
          
        }
        System.out.println();

        all.add(res);

        num_solutions++;
        
      } while (solver.nextSolution() == Boolean.TRUE);

      System.out.println(all);
      System.out.println("\nIt was " + num_solutions + " solutions.");

    }  else {

      System.out.println("No solution.");

    }

  }


  public static void main(String args[]) {

    new MoneyChange().execute(args);

  }


}

