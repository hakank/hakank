package org.jcp.jsr331.hakan;

/**
  *
  * Set covering problem in JSR-331.
  *
  * Example 9.1-2 from
  * Taha "Operations Research - An Introduction",
  * page 354ff.
  * Minimize the number of security telephones in street
  * corners on a campus.
  *
  * Model by Hakan Kjellerstrand (hakank@bonetmail.com)
  * Also see http://www.hakank.org/jsr_331/
  *
  */

import javax.constraints.*;

import java.io.*;
import java.util.*;
import java.text.*;

public class SetCovering2 {

  Problem p = ProblemFactory.newProblem("SetCovering2");
  int n;

  public static void main(String[] args) {
    SetCovering2 pp = new SetCovering2();
    pp.define();
    pp.solve();
  }
    

  // Problem definition    
  public void define() {
        
    //
    // data
    //
    n = 8;            // maximum number of corners
    int num_streets = 11; // number of connected streets

    // corners of each street
    // Note: 1-based (handled below)
    int[][] corner = {{1,2},
                      {2,3},
                      {4,5},
                      {7,8},
                      {6,7},
                      {2,6},
                      {1,6},
                      {4,7},
                      {2,4},
                      {5,8},
                      {3,5}};



    //
    // variables
    // 
    Var[] x = p.variableArray("x", 0, 1, n);
    Var z = p.sum(x);
    z.setName("z");
    p.add(z);


    //
    // constraints
    // 

    // ensure that all streets are covered
    for(int i = 0; i < num_streets; i++) {
      Var[] b = new Var[2];
      b[0] = x[corner[i][0] - 1];
      b[1] = x[corner[i][1] - 1];
      p.post(b,">=", 1);

    }



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
    strategy.setVarSelectorType(VarSelectorType.MIN_DOMAIN);
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
    strategy.setValueSelectorType(ValueSelectorType.MIN);
    // strategy.setValueSelectorType(ValueSelectorType.MAX);
    // strategy.setValueSelectorType(ValueSelectorType.MIN_MAX_ALTERNATE);
    // strategy.setValueSelectorType(ValueSelectorType.MIDDLE);
    // strategy.setValueSelectorType(ValueSelectorType.MEDIAN);
    // strategy.setValueSelectorType(ValueSelectorType.RANDOM);
    // strategy.setValueSelectorType(ValueSelectorType.MIN_IMPACT);
    // strategy.setValueSelectorType(ValueSelectorType.CUSTOM);
        
    // solver.addSearchStrategy(new StrategyLogVariables(solver)); 
        
    //
    // solve
    //
    // solver.traceExecution(true);
        
    Solution solution = solver.findOptimalSolution(Objective.MINIMIZE, p.getVar("z"));

    if (solution == null) {

      System.out.println("No solution");

    } else {

      
      System.out.println("z: " + solution.getValue("z"));
      System.out.print("x: ");
      for(int i = 0; i < n; i++) {
        System.out.print(solution.getValue("x-"+i) + " ");
      }
      System.out.println("\n");

      // solution.log();

    }
    solver.logStats();
  }

}
