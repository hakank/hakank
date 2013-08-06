package org.jcp.jsr331.hakan;


/**
  *
  * To num (channeling between array and value) in JSR-331.
  *
  *
  * This model was created by Hakan Kjellerstrand (hakank@bonetmail.com)
  * Also, see my JSR-331 page: http://www.hakank.org/jsr_331/ 
  *
  */
import javax.constraints.*;

import java.io.*;
import java.util.*;
import java.text.*;

public class ToNum {

  int n;

  Problem p = ProblemFactory.newProblem("ToNum");

  /**
   * toNum(arr, num, base)
   *
   * channel between the array arr and number num 
   * given a base.
   *
   */
  public void toNum(Var[] a, Var num, int base) {
    int len = a.length;

    Var[] tmp = new Var[len];
    for(int i = 0; i < len; i++) {
      tmp[i] = a[i].multiply((int)Math.pow(base,(len-i-1)));
    }

    // Ensure that sum(tmp) == num
    p.post(tmp, "=", num);
        
  }

  // main
  public static void main(String[] args) {

    ToNum pp = new ToNum();
    pp.define();
    pp.solve();
  }
    

  // Problem definition    
  public void define() {

    n = 5;
    int base = 10;

    //
    // variables
    //
    Var[] x = p.variableArray("x", 0, base-1, n);
    Var num = p.variable("num", 0, (int)Math.pow(base, n) - 1);

    //
    // constraints
    //
    p.postAllDifferent(x);

    toNum(x, num, base);

  }
    
    
  public void solve() {
    //
    // search
    //
    Solver solver = p.getSolver();
    SearchStrategy strategy = solver.getSearchStrategy();

    // strategy.setVarSelectorType(VarSelectorType.INPUT_ORDER);
    // strategy.setVarSelectorType(VarSelectorType.MIN_VALUE);
    // strategy.setVarSelectorType(VarSelectorType.MAX_VALUE);
    // strategy.setVarSelectorType(VarSelectorType.MIN_DOMAIN);
    // strategy.setVarSelectorType(VarSelectorType.MIN_DOMAIN_MIN_VALUE);
    // strategy.setVarSelectorType(VarSelectorType.MIN_DOMAIN_RANDOM);
    // strategy.setVarSelectorType(VarSelectorType.RANDOM);
    // strategy.setVarSelectorType(VarSelectorType.MIN_DOMAIN_MAX_DEGREE);
    // strategy.setVarSelectorType(VarSelectorType.MIN_DOMAIN_OVER_DEGREE);
    // strategy.setVarSelectorType(VarSelectorType.MIN_DOMAIN_OVER_WEIGHTED_DEGREE);
    // strategy.setVarSelectorType(VarSelectorType.MAX_WEIGHTED_DEGREE);
    // strategy.setVarSelectorType(VarSelectorType.MAX_IMPACT);
    // strategy.setVarSelectorType(VarSelectorType.MAX_DEGREE);
    // strategy.setVarSelectorType(VarSelectorType.MAX_REGRET);
        
        
        
        
    // strategy.setValueSelectorType(ValueSelectorType.IN_DOMAIN);
    // strategy.setValueSelectorType(ValueSelectorType.MIN);
    // strategy.setValueSelectorType(ValueSelectorType.MAX);
    // strategy.setValueSelectorType(ValueSelectorType.MIN_MAX_ALTERNATE);
    // strategy.setValueSelectorType(ValueSelectorType.MIDDLE);
    // strategy.setValueSelectorType(ValueSelectorType.MEDIAN);
    // strategy.setValueSelectorType(ValueSelectorType.RANDOM);
    // strategy.setValueSelectorType(ValueSelectorType.MIN_IMPACT);
    // strategy.setValueSelectorType(ValueSelectorType.CUSTOM);
        
    //
    // tracing
    //
    // solver.addSearchStrategy(new StrategyLogVariables(solver)); 
    // solver.traceExecution(true);

    //
    // solve
    //        
    int num_sols = 0;
    SolutionIterator iter = solver.solutionIterator();
    int max_num_solutions = 10;
    while (iter.hasNext()) {
      num_sols++;
      Solution s = iter.next();
      System.out.print("\nnum: " + s.getValue("num") + " x: ");
      for(int i = 0; i < n; i++) {
        System.out.print(s.getValue("x-"+i) + " ");
      }
      
      // System.out.println();
      // s.log();
      if (num_sols >= max_num_solutions) {
        break;
      }
    }
    System.out.println("\nIt was " + num_sols + " solutions");

    solver.logStats();
  }

}
