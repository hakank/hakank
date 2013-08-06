package org.jcp.jsr331.hakan;

/**
  *
  * Set covering problem in JSR-331.
  *
  * Placing of firestations, from Winston 'Operations Research',
  * page 486.
  *
  * Model by Hakan Kjellerstrand (hakank@bonetmail.com)
  * Also see http://www.hakank.org/jsr_331/
  *
  */

import javax.constraints.*;

import java.io.*;
import java.util.*;
import java.text.*;

public class SetCovering {

  Problem p = ProblemFactory.newProblem("SetCovering");
  int num_cities;

  public static void main(String[] args) {
    SetCovering pp = new SetCovering();
    pp.define();
    pp.solve();
  }
    

  // Problem definition    
  public void define() {
        
    //
    // data
    //
    int min_distance = 15;
    num_cities = 6;

    int[][] distance = {{ 0,10,20,30,30,20},
                        {10, 0,25,35,20,10},
                        {20,25, 0,15,30,20},
                        {30,35,15, 0,15,25},
                        {30,20,30,15, 0,14},
                        {20,10,20,25,14, 0}};


    //
    // variables
    // 
    Var[] x = p.variableArray("x", 0, 1, num_cities);
    Var z = p.variable("z", 0, num_cities); // p.sum(x);


    //
    // constraints
    // 

    // ensure that all cities are covered
    Var zero = p.variable("zero", 0, 0); // constant
    for(int i = 0; i < num_cities; i++) {
      Var[] b = new Var[num_cities]; 
      for(int j = 0; j < num_cities; j++) {
        if (distance[i][j] <= min_distance) {
          b[j] = x[j];
        } else {
          b[j] = zero;
        }
      }

      p.post(b,">=", 1);

    }

    p.post(p.sum(x),"=", z);


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
      for(int i = 0; i < num_cities; i++) {
        System.out.print(solution.getValue("x-"+i) + " ");
      }
      System.out.println("\n");

      // solution.log();

    }
    solver.logStats();
  }

}
