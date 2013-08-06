package org.jcp.jsr331.hakan;

/**
  *
  * Global constraint circuit (decomposition) in JSR-331.
  *
  * See Global Constraint Catalog
  * http://www.emn.fr/z-info/sdemasse/gccat/Ccircuit.html
  *
  * This model was created by Hakan Kjellerstrand (hakank@bonetmail.com)
  * Also, see my JSR-331 page: http://www.hakank.org/jsr_331/ 
  *
  */
import javax.constraints.*;

import java.io.*;
import java.util.*;
import java.text.*;

public class Circuit_test {
  
  int n;
  
  Problem p = ProblemFactory.newProblem("Circuit_test");
  
  /**
   * circuit(Var[] v)
   *
   * A decomposition of the global constraint circuit,
   * based on some observations of the orbits in an array.
   *
   * Note: The domain of x must be 0..n-1 (not 1..n) since
   * Java is 0-based.
   *
   */
  public void Circuit(Var[] x) {
    int len = x.length;

    Var[] z = p.variableArray("z", 0, n-1, n);

    p.postAllDifferent(x);
    p.postAllDifferent(z);

    // put the orbit of x[0] in z[0..n-1]
    p.post(z[0],"=",x[0]);
    for(int i = 1; i < n-1; i++) {
      p.postElement(x, z[i-1], "=", z[i]);
    }

    // z may not be 0 for i < n-1
    for(int i = 1; i < n - 1; i++) {
      p.post(z[i], "!=", 0);
    }
        
    // when i = n-1 it must be 0
    p.post(z[n-1], "=", 0);

        
  }

  // main
  public static void main(String[] args) {

    Circuit_test pp = new Circuit_test();
    pp.define();
    pp.solve();

  }
    

  // Problem definition    
  public void define() {

    n = 5;

    //
    // variables
    //
    Var[] x = p.variableArray("x", 0, n-1, n);

    //
    // constraints
    //
    Circuit(x);

  }
    
    
  public void solve() {
    //
    // search
    //
    Solver solver = p.getSolver();
    SearchStrategy strategy = solver.getSearchStrategy();

    strategy.setVarSelectorType(VarSelectorType.INPUT_ORDER);
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
        
    strategy.setValueSelectorType(ValueSelectorType.IN_DOMAIN);
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
    // solver.traceExecution(true);

    //
    // solve
    //        
    int num_sols = 0;
    SolutionIterator iter = solver.solutionIterator();
    while (iter.hasNext()) {
      num_sols++;
      Solution s = iter.next();
      for(int i = 0; i < n; i++) {
        System.out.print(s.getValue("x-"+i) + " ");
      }
      System.out.println();
      // s.log();
    }

    System.out.println("It was " + num_sols + " solutions");

    solver.logStats();
  }

}
