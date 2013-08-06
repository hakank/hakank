package org.jcp.jsr331.hakan;

/**
 *
 * Alternative version of Sudoku in JSR-311.
 *
 *
 * Model by Hakan Kjellerstrand (hakank@bonetmail.com)
 * Also see http://www.hakank.org/jsr_331/
 *
 */

import javax.constraints.*;

/**
 * Sudoku Description: 
 * A 81 cells square grid is divided in 9 smaller blocks of 9 cells
 * (3 x 3). Some of the 81 are filled with one digit. The aim of the puzzle is
 * to fill in the other cells, using digits except 0, such as each digit appears
 * once and only once in each row, each column and each smaller block. The
 * solution is unique.
 */

public class Sudoku2 {

    Var[][] x;
    int n;

    int[][] data1 = new int[][] {
        {0,6,9,0,0,7,0,0,5},
        {0,5,0,0,0,4,0,2,0},
        {4,0,0,0,5,0,1,0,0},
        {8,0,5,0,0,0,6,0,0},
        {6,7,0,2,9,5,0,1,4},
        {0,0,1,0,0,0,7,0,9},
        {0,0,6,0,1,0,0,0,7},
        {0,1,0,4,0,0,0,8,0},
        {5,0,0,3,0,0,2,6,0}
    };

    int[][] data = new int[][] {
        {7,0,0,1,0,6,8,2,0},
        {0,0,3,0,0,0,0,0,0},
        {0,0,8,0,9,0,4,0,0},
        {0,0,7,9,0,0,0,0,0},
        {0,0,0,0,5,3,0,1,0},
        {1,0,9,2,0,0,6,0,0},
        {0,0,0,0,0,0,9,3,0},
        {0,0,0,5,0,0,0,0,2},
        {0,0,0,4,0,0,0,7,0}
    };


    /*

      Solution:
      7 9 5 1 4 6 8 2 3 
      6 4 3 8 2 5 7 9 1 
      2 1 8 3 9 7 4 6 5 
      5 2 7 9 6 1 3 8 4 
      8 6 4 7 5 3 2 1 9 
      1 3 9 2 8 4 6 5 7 
      4 5 1 6 7 2 9 3 8 
      9 7 6 5 3 8 1 4 2 
      3 8 2 4 1 9 5 7 6 

    */


    Problem p = ProblemFactory.newProblem("Sudoku2");

    public static void main(String[] args) {

        Sudoku2 pp = new Sudoku2();
        pp.define();
        pp.solve();


    }

    public void define() {

        n = 9;

        // create n x n square
        x = new Var[n][n];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                String iName = "x" + "[" + i + "," + j	+ "]";
                x[i][j] = p.variable(iName, 1, n); 
            }
        }

        // get the hints
        for (int i = 0; i < n; i++) {
            for(int j=0; j < n; j++) {
                if (data[i][j] != 0) {
                    try {
                        p.post(x[i][j],"=",data[i][j]);
                    } catch(Exception e) {
                        p.log("Fail at i="+i+" j="+j);
                        System.exit(-1);
                    }
                }
            }
        }

        // rows and columns
        for (int i = 0; i < n; i++) {
            // Var[] row = new Var[n];
            // Var[] col = new Var[n];
            Var[] row = p.variableArray("row"+i, 1, n, n);
            Var[] col = p.variableArray("col"+i, 1, n, n);

            for (int j = 0; j < n; j++) {
                // row[j] = x[i][j];
                // col[j] = x[j][i];
                p.post(row[j], "=", x[i][j]);
                p.post(col[j], "=", x[j][i]);

            }
            try {
                p.postAllDifferent(row);
                p.postAllDifferent(col);
            } catch(Exception e) {
                p.log("Fail at row/col i="+i);
                System.exit(-1);
            }
        }

                
        // blocks
        int s = 3;
        for (int v = 0; v < n; v++) {
            for (int i = 0; i < s; i++) {
                for (int j = 0; j < s; j++) {
                    // gather the blocks
                    // Var[] block = new Var[n];
                    Var[] block = p.variableArray("block"+i+" "+j, 1, n, n);
                    for (int i1 = 0; i1 < s; i1++) {
                        for (int j1 = 0; j1 < s; j1++) {
                            // block[i1*3+j1] = x[i*s+i1][j*s+j1];
                            p.post(block[i1*3+j1], "=", x[i*s+i1][j*s+j1]);
                        }
                    }
                    try {
                        p.postAllDifferent(block);
                    } catch(Exception e) {
                        p.log("Fail at i="+i+" j="+j);
                        System.exit(-1);
                    }
                }
            }
        }
       
        
    }

    public void solve() {

        Solver solver = p.getSolver();
        
        SearchStrategy strategy = solver.getSearchStrategy();

        // ========= Problem Resolution ==================
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
        
        // Make sure it is a unique solution.
        int num_solutions = 0;
        SolutionIterator iter = solver.solutionIterator();
        while (iter.hasNext()) {
            num_solutions++;
            Solution solution = iter.next();
            for (int i = 0; i < n; i++) {
                String str = new String();
                for (int j = 0; j < n; j++) {
                    int value = solution.getValue("x["+i+"," +j+"]");
                    str = str + value + " ";
                }
                p.log(str);
            }
            p.log("\n");       
            // break;
        }

        if (num_solutions != 1) {
            p.log("Weird, it has " + num_solutions + "!");
        }

        solver.logStats();
    }
}