package org.jcp.jsr331.hakan;

/**
 *
 * Coin Grid problem in JSR331.
 *
 * Problem from 

 * Tony Hürlimann: "A coin puzzle - SVOR-contest 2007"
 * http://www.svor.ch/competitions/competition2007/AsroContestSolution.pdf
 *
 * """
 * In a quadratic grid (or a larger chessboard) with 31x31 cells, one should place coins in such a
 * way that the following conditions are fulfilled:
 *    1. In each row exactly 14 coins must be placed.
 *    2. In each column exactly 14 coins must be placed.
 *    3. The sum of the quadratic horizontal distance from the main diagonal of all cells
 *       containing a coin must be as small as possible.
 *    4. In each cell at most one coin can be placed.
 * The description says to place 14x31 = 434 coins on the chessboard each row containing 14
 * coins and each column also containing 14 coins.
 * """
 *
 * Cf the LPL model:
 * http://diuflx71.unifr.ch/lpl/GetModel?name=/puzzles/coin
 * 
 *
 * Compare with the following models:
 * - Choco: http://www.hakank.org/choco/CoinsGrid.java
 * - Comet: http://www.hakank.org/comet/coins_grid.co
 * - ECLiPSE: http://www.hakank.org/eclipse/coins_grid.ecl
 * - Gecode: http://www.hakank.org/gecode/coins_grid.cpp
 * - Gecode/R: http://www.hakank.org/gecode_r/coins_grid.rb
 * - Google CP Solver: http://www.hakank.org/google_or_tools/coins_grid.py
 * - JaCoP: http://www.hakank.org/JaCoP/CoinsGrid.java
 * - MiniZinc: http://www.hakank.org/minizinc/coins_grid.mzn
 * - SICStus: http://www.hakank.org/sicstus/coins_grid.pl
 * - Tailor/Essence': http://www.hakank.org/tailor/coins_grid.eprime
 * - Zinc: http://www.hakank.org/minizinc/coins_grid.zinc
 *
 * Model by Hakan Kjellerstrand (hakank@bonetmail.com)
 * Also see http://www.hakank.org/jsr_331/
 *
 */

import javax.constraints.*;

import java.io.*;
import java.util.*;
import java.text.*;

public class CoinsGrid {

    // used in solve()
    Var[][] x;
    Var z;

    int n; 
    int c; 

    Problem p = ProblemFactory.newProblem("CoinsGrid");

    public static void main(String[] args) {

       // original problem: 
        // int tmp_n = 31
        // tmp_c = 14

        // defaults to a small problem
        int tmp_n = 8;
        int tmp_c = 3;
        System.out.println("args.length:" + args.length);
        if (args.length == 2) {
            tmp_n = Integer.parseInt(args[0]);
            tmp_c = Integer.parseInt(args[1]);
        }

        System.out.println("Using n = " + tmp_n + " c = " + tmp_c);
  
        CoinsGrid pp = new CoinsGrid();
        pp.define(tmp_n, tmp_c);
        pp.solve();
    }
    

    // Problem definition    
    public void define(int nn, int cc) {
        
        n = nn;
        c = cc; 
        
        // define variables
        Var v_c = p.variable("c", c, c); // "constant"
        x = new Var[n][n];
        for(int i = 0; i < n; i++) {
            x[i] = p.variableArray("x"+i, 0, 1, n);
        }

        z = p.variable("z", 0, 10000);
        Var[] z_array = new Var[n*n];

        // define and post constraints
        try {

            for(int i = 0; i < n; i++) {
                ArrayList<Var> col = new ArrayList<Var>();
                for(int j = 0; j < n; j++) {
                    p.post(p.sum(x[i]), "=", v_c);
                    col.add(x[j][i]);
                }
                p.post(col.toArray(new Var[1]), "=", v_c);
            }
            
            /* 
             * to minimize: quadratic horizonal distance, i.e.
             *    sum over x[i][j]*(abs(i-j))*(abs(i-j)) 
             *
             * z should be 13668 for n = 31 and c = 14
             *
             */

            for(int i = 0; i < n; i++) {
                for(int j = 0; j < n; j++) {
                    int abs_i_j = Math.abs(i-j)*Math.abs(i-j);
                    z_array[i*n + j] = p.variable("z_" + i +"_" + j, 0, 10000);
                    p.post(x[i][j].multiply(abs_i_j), "=", z_array[i*n + j]);
                    
                }
            }
            p.post(p.sum(z_array), "=", z);

            
        } catch (Exception e) {
            p.log("Error posting constraints: " + e);
            System.exit(-1);
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
        // strategy.setValueSelectorType(ValueSelectorType.MIN);
        strategy.setValueSelectorType(ValueSelectorType.MAX);
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
        Solution solution = solver.findOptimalSolution(Objective.MINIMIZE, p.getVar("z"));
        if (solution == null) {
            p.log("No solution");
        } else {
            solution.log();

            if (z.isBound()) {
                System.out.println("z: " + z.getValue());
            } else {
                System.out.println("z: " + z);
            }
            for(int i = 0; i < n; i++) {
                for(int j = 0; j < n; j++) {
                    if (x[i][j].isBound()) {
                        System.out.print(x[i][j].getValue() + " ");
                    } else {
                        System.out.print(x[i][j] + " ");
                    }
                }
                System.out.println();
            }
        }
        solver.logStats();
    }

}
