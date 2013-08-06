package org.jcp.jsr331.hakan;

/**
 *
 * 17x17 problem in JSR-331.
 *
 * Problem from Karsten Konrad:
 * http://lookforlight.tumblr.com/post/996786415/lets-do-real-cp-forbiddenassignment
 * """
 * The n x m grid is c-colorable if there is a way
 * to c-color the vertices of the n x m grid so that
 * there is no rectangle with all four corners the
 * same color. (The rectangles I care about have the
 * sides parallel to the x and y axis.)
 * 
 * Is there a 17x17 solution?
 * see: http://blog.computationalcomplexity.org/2009/11/17x17-challenge-worth-28900-this-is-not.html
 *  """
 *
 * Compare with the following models:
 * - MiniZinc: http://hakank.org/minizinc/17_b.mzn
 *             http://hakank.org/minizinc/17_b3.mzn
 * - Comet: http://hakank.org/comet/17_b.co
 * - Zinc: http://hakank.org/minizinc/17_b3.zinc
 * 
 *
 * Model by Hakan Kjellerstrand (hakank@bonetmail.com)
 * Also see http://www.hakank.org/jsr_331/
 */
import javax.constraints.*;

import java.io.*;
import java.util.*;
import java.text.*;

public class Grid17x17 {

    int num_rows;
    int num_cols;
    int num_colors;

    Var[] space_flatten;
  Problem p = ProblemFactory.newProblem("Grid 17x17");

    // main
    public static void main(String[] args) {

        int num_rows_in = 6;
        int num_cols_in = 6 ;
        int num_colors_in  = 4;

        if (args.length >= 1) {
            num_rows_in = Integer.parseInt(args[0]);
        }

        if (args.length >= 2) {
            num_cols_in = Integer.parseInt(args[1]);
        }

        if (args.length >= 3) {
            num_colors_in = Integer.parseInt(args[2]);
        }

        System.out.println("\nnum_rows: " + num_rows_in + 
                           " num_cols: " + num_cols_in + 
                           " num_colors: " + num_colors_in + 
                           "\n");

        Grid17x17 p = new Grid17x17();
        p.define(num_rows_in, num_cols_in, num_colors_in);
        p.solve();

    }


    // Problem definition    
    public void define(int num_rows_in, int num_cols_in, int num_colors_in) {
        num_rows = num_rows_in;
        num_cols = num_cols_in;
        num_colors = num_colors_in;

        Var[][] space = new Var[num_rows][num_cols];
        space_flatten = new Var[num_rows*num_cols];
        for(int i = 0; i < num_rows; i++) {
            for(int j = 0; j < num_cols; j++) {
                space[i][j] = p.variable("space-"+i+"-"+j, 0, num_colors-1);
                space_flatten[i*num_cols+j] = space[i][j];
            }
        }
        

        int[] values = new int[num_colors];
        int[] cardMin = new int[num_colors];
        int[] cardMax = new int[num_colors];
        for(int i = 0; i < num_colors; i++) {
            values[i] = i;
            cardMin[i] = 0;
            cardMax[i] = num_colors-1;
        }
        for(int r = 0; r < num_rows; r++) {
            for(int r2 = 0; r2 < r; r2++) {
                for(int c = 0; c < num_cols; c++) {
                    for(int c2 = 0; c2 < c; c2++) {
                        Var[] tmp = {space[r][c],
                                     space[r2][c],
                                     space[r][c2],
                                     space[r2][c2]};
                        p.postGlobalCardinality(tmp, values, cardMin, cardMax);
                    }
                }
            }
        }

        // symmetry breaking
        p.post(space[0][0], "=", 0);

    }
    
    
    public void solve() {
        //
        // search
        //
        Solver solver = p.getSolver();
        SearchStrategy strategy = solver.getSearchStrategy();
        strategy.setVars(space_flatten);

        // strategy.setVarSelectorType(VarSelectorType.INPUT_ORDER);
        strategy.setVarSelectorType(VarSelectorType.MIN_VALUE);
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
        strategy.setValueSelectorType(ValueSelectorType.MIN_MAX_ALTERNATE);
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
        while (iter.hasNext()) {
            num_sols++;
            Solution s = iter.next();
            // s.log();

            for(int i = 0; i < num_rows; i++) {
                for(int j = 0; j < num_cols; j++) {
                    System.out.print(s.getValue("space-"+i+"-"+j) + " ");
                }
                System.out.println();
            }
            System.out.println();

            if (num_sols > 0) {
                break;
            }

        }

        solver.logStats();
    }

}
