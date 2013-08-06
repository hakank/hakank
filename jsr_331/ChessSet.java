package org.jcp.jsr331.hakan;

/**
 *
 * Chess set problem in JSR-331.
 *
 * Applications of Optimization with XPress-MP.pdf,
 * pages 11 (the problem is presented from page 7) 
 * where the objective is to produce small and large
 * chess sets.
 *
 * Compare with the following model:
 * - http://www.hakank.org/minizinc/chess_set.mzn
 *
 * Model by Hakan Kjellerstrand (hakank@bonetmail.com)
 * Also see http://www.hakank.org/jsr_331
 *
 */

import javax.constraints.*;

import java.io.*;
import java.util.*;
import java.text.*;

public class ChessSet {

    Var[] x;
    Var x_profit;

    Problem p = ProblemFactory.newProblem("ChessSet");

    // main
    public static void main(String[] args) {

        ChessSet pp = new ChessSet();
        pp.define();
        pp.solve();

    }
    

    // Problem definition    
    public void define() {

        int n = 2;
        int[] profit = {5,20};
        int[][] costs = {{1,3},
                         {3,2}};
        int[] cost_limits = {200, 160};

        x = p.variableArray("x", 0, 100, n);

        for(int i = 0; i < n; i++) {
            p.post(costs[i], x, "<=", cost_limits[i]);
        }
        
        Var x_profit = p.variable("x_profit", 0, 10000);
        p.post(profit, x, "=", x_profit);

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
        Solution s = solver.findOptimalSolution(Objective.MAXIMIZE, p.getVar("x_profit"));

        if (s == null) {
            p.log("No solution");
        } else {

            System.out.println("\nsmall set: " + s.getValue("x-0") + " large set: " + s.getValue("x-1"));
            System.out.println("profit: " + s.getValue("x_profit"));
            System.out.println();

            // s.log();
        }

        solver.logStats();
    }

}
