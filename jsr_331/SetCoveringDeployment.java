package org.jcp.jsr331.hakan;

/**
  *
  * Set covering deployment problem in JSR-331.
  *
  * See
  * http://mathworld.wolfram.com/SetCoveringDeployment.html
  *
  * Model by Hakan Kjellerstrand (hakank@bonetmail.com)
  * Also see http://www.hakank.org/jsr_331/
  *
  */

import javax.constraints.*;

import java.io.*;
import java.util.*;
import java.text.*;

public class SetCoveringDeployment {

  Problem p = ProblemFactory.newProblem("SetCoveringDeployment");
  int n;

  // From http://mathworld.wolfram.com/SetCoveringDeployment.html
  String[] countries = {"Alexandria",
                        "Asia Minor",
                        "Britain",
                        "Byzantium",
                        "Gaul",
                        "Iberia",
                        "Rome",
                        "Tunis"};


  public static void main(String[] args) {

    SetCoveringDeployment pp = new SetCoveringDeployment();
    pp.define();
    pp.solve();


  }
    

  // Problem definition    
  public void define() {

    //
    // data
    // 


    n = countries.length;

    // the incidence matrix (neighbours)
    int[][] mat = {{0, 1, 0, 1, 0, 0, 1, 1},
                   {1, 0, 0, 1, 0, 0, 0, 0},
                   {0, 0, 0, 0, 1, 1, 0, 0},
                   {1, 1, 0, 0, 0, 0, 1, 0},
                   {0, 0, 1, 0, 0, 1, 1, 0},
                   {0, 0, 1, 0, 1, 0, 1, 1},
                   {1, 0, 0, 1, 1, 1, 0, 1},
                   {1, 0, 0, 0, 0, 1, 1, 0}};


    //
    // variables
    // 

    // First army
    Var[] x = p.variableArray("x", 0, 1, n);
    // Second army
    Var[] y = p.variableArray("y", 0, 1, n);

    // total number of armies
    Var num_armies = p.sum(x).plus(p.sum(y));
    num_armies.setName("num_armies");
    p.add(num_armies);


    //
    // constraints
    // 

    //
    //  Constraint 1: There is always an army in a city
    //                (+ maybe a backup)
    //                Or rather: Is there a backup, there
    //                must be an an army
    //
    for(int i = 0; i < n; i++) {
      p.post(x[i],">=", y[i]);
    }

    //
    // Constraint 2: There should always be an backup
    //               army near every city
    //
    Var zero = p.variable("zero", 0, 0);
    for(int i = 0; i < n; i++) {
      Var[] count_neighbours = new Var[n];
      for(int j = 0; j < n; j++) {
        if (mat[i][j] == 1) {
          count_neighbours[j] = y[j];
        } else {
          count_neighbours[j] = zero;
        }
      }

      p.post(x[i].plus(p.sum(count_neighbours)),">=",1);

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
        
    // solver.addSearchStrategy(new StrategyLogVariables(solver)); 
        
    //
    // solve
    //
    // solver.traceExecution(true);
        
    Solution solution = solver.findOptimalSolution(Objective.MINIMIZE, p.getVar("num_armies"));

    if (solution == null) {

      System.out.println("No solution");

    } else {

      System.out.println("num_armies: " + solution.getValue("num_armies"));
      for(int i = 0; i < n; i++) {
        System.out.print(solution.getValue("x-"+i) + " ");
      }
      System.out.println();
      for(int i = 0; i < n; i++) {
        System.out.print(solution.getValue("y-"+i) + " ");
      }
      System.out.println();


      for(int i = 0; i < n; i++) {
        if (solution.getValue("x-"+i) == 1) {
          System.out.print("\nArmy: " + countries[i] + " ");
        }
        if (solution.getValue("y-"+i) == 1) {
          System.out.println("Reserve army: " + countries[i]);
        }
      }
      System.out.println();

    }
    solver.logStats();
  }

}
