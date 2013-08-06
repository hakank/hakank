package org.jcp.jsr331.hakan;

/**
 *
 * Strimko problem in JSR-331.
 *
 * From 
 * 360: A New Twist on Latin Squares
 * http://threesixty360.wordpress.com/2009/08/04/a-new-twist-on-latin-squares/
 * """
 * The idea is simple: each row and column of an nxn grid must contain 
 * the number 1, 2, ... n exactly once (that is, the grid must form a 
 * Latin square), and each "stream" (connected path in the grid) must 
 * also contain the numbers 1, 2, ..., n exactly once.
 * """
 *
 * For more information, see:
 * - http://www.strimko.com/
 * - http://www.strimko.com/rules.htm
 * - http://www.strimko.com/about.htm
 * - http://www.puzzlersparadise.com/Strimko.htm
 *
 * Model by Hakan Kjellerstrand (hakank@bonetmail.com)
 * Also see http://www.hakank.org/jsr_331/
 */
import javax.constraints.*;

import java.io.*;
import java.util.*;
import java.text.*;

public class Strimko2 {

  int n;
  Problem p = ProblemFactory.newProblem("Strimko2");

  // main
  public static void main(String[] args) {

    Strimko2 pp = new Strimko2();
    pp.define();
    pp.solve();

  }


  // Problem definition    
  public void define() {

    //
    // data
    //
    int[][] streams = {{1,1,2,2,2,2,2},
                       {1,1,2,3,3,3,2},
                       {1,4,1,3,3,5,5},
                       {4,4,3,1,3,5,5},
                       {4,6,6,6,7,7,5},
                       {6,4,6,4,5,5,7},
                       {6,6,4,7,7,7,7}};

    // Note: This is 1-based
    int[][] placed = {{2,1,1},
                      {2,3,7},
                      {2,5,6},
                      {2,7,4},
                      {3,2,7},
                      {3,6,1},
                      {4,1,4},
                      {4,7,5},
                      {5,2,2},
                      {5,6,6}};

    n = streams.length;
    int num_placed = placed.length;


    // 
    // variables
    // 
    Var[][] x = new Var[n][n];
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        x[i][j] = p.variable("x[" + i + "," + j + "]", 1, n);
      }
    }


    //
    // constraints
    //

    // all rows and columns must be unique, i.e. a Latin Square
    for(int i = 0; i < n; i++) {
      Var[] row = new Var[n];
      Var[] col = new Var[n];
      for(int j = 0; j < n; j++) {
        row[j] = x[i][j];
        col[j] = x[j][i];
      }
      p.postAllDifferent(row);
      p.postAllDifferent(col);
    }


    // streams
    for(int s = 1; s <= n; s++) {
      ArrayList<Var> tmp = new ArrayList<Var>();
      for(int i = 0; i < n; i++) {
        for(int j = 0; j < n; j++) {
          if (streams[i][j] == s) {
            tmp.add(x[i][j]);
          }
        }
      }
      p.postAllDifferent(tmp.toArray(new Var[1]));
    }


    // placed
    for(int i = 0; i <  num_placed; i++) {
      // note: here we also adjust to 0-based
      p.post(x[placed[i][0] - 1][placed[i][1] - 1],"=", placed[i][2]);
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
      // s.log();

      for(int i = 0; i < n; i++) {
        for(int j = 0; j < n; j++) {
          System.out.print(s.getValue("x["+i+","+j+"]") + " ");
        }
        System.out.println();
      }
      System.out.println();

    }

    solver.logStats();
  }

}
