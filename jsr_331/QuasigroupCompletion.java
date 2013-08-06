package org.jcp.jsr331.hakan;

/**
 *
 * Quasigroup Completion problem in JSR-331.
 *
 * See 
 * Carla P. Gomes and David Shmoys:
 * "Completing Quasigroups or Latin Squares: Structured Graph Coloring Problem"
 *
 *
 * See also
 * Ivars Peterson "Completing Latin Squares"
 * http://www.maa.org/mathland/mathtrek_5_8_00.html
 * """
 * Using only the numbers 1, 2, 3, and 4, arrange four sets of these numbers into 
 * a four-by-four array so that no column or row contains the same two numbers. 
 * The result is known as a Latin square.
 * ...
 * The so-called quasigroup completion problem concerns a table that is correctly 
 * but only partially filled in. The question is whether the remaining blanks in 
 * the table can be filled in to obtain a complete Latin square (or a proper 
 * quasigroup multiplication table).
 * """
 * 
 * Compare with the following models:
 * - Choco: http://www.hakank.org/choco/QuasigroupCompletion.java
 * - Comet: http://www.hakank.org/comet/quasigroup_completion.co
 * - ECLiPSE: http://www.hakank.org/eclipse/quasigroup_completion.ecl
 * - Gecode: http://www.hakank.org/gecode/quasigroup_completion.cpp
 * - Gecode/R: http://www.hakank.org/gecode_r/quasigroup_completion.rb
 * - Google CP Solver: http://www.hakank.org/google_or_tools/quasigroup_completion.py
 * - JaCoP: http://www.hakank.org/JaCoP/QuasigroupCompletion.java
 * - MiniZinc: http://www.hakank.org/minizinc/quasigroup_completion.mzn
 * - SICStus: http://www.hakank.org/sicstus/quasigroup_completion.pl
 * - Tailor/Essence': http://www.hakank.org/tailor/quasigroup_completion.eprime
 * - Zinc: http://www.hakank.org/minizinc/quasigroup_completion.zinc
 *
 * Model by Hakan Kjellerstrand (hakank@bonetmail.com)
 * Also see http://www.hakank.org/jsr_331/
 *
 */
import javax.constraints.*;

import java.io.*;
import java.util.*;
import java.text.*;

public class QuasigroupCompletion {

    int n;
    Var[][] x;
    int[][] matrix;
  Problem p = ProblemFactory.newProblem("QuasigroupCompletion");

    //
    // k_all_different (make x a latin square)
    //
    public void k_all_different(Var[][] y, int n) {

        for(int i = 0; i < n; i++) {
            ArrayList<Var> col = new ArrayList<Var>();
            for(int j = 0; j < n; j++) {
                p.postAllDifferent(y[i]); // rows
                col.add(y[j][i]);
            }
            p.postAllDifferent(col.toArray(new Var[1])); // columns
        }

    } // end k_all_different


    // main
    public static void main(String[] args) {

        QuasigroupCompletion pp = new QuasigroupCompletion();
        String file = "";
        if (args.length > 0) {
            file = args[0];
            pp.readFile(file);
        }
        pp.define();
        pp.solve();

    }
    

    // Problem definition    
    public void define() {

        /* // _many_ solution!
        int[][] matrix = {{0,0,0,1,0,0,0,0,0,0},
                          {0,0,1,0,0,0,0,0,0,0},
                          {0,1,0,0,0,2,0,0,0,0},
                          {1,0,0,0,2,0,0,0,0,0},
                          {0,0,0,2,1,0,0,0,0,0},
                          {0,0,2,0,0,1,0,0,0,0},
                          {0,0,0,0,0,0,1,0,0,0},
                          {0,0,0,0,0,0,0,1,0,2},
                          {0,0,0,0,0,0,0,0,2,0},
                          {0,0,0,0,0,0,0,2,0,0}};
        */

        
        if (matrix == null) {
            /* Example from Ruben Martins and Inès Lynce
               Breaking Local Symmetries in Quasigroup Completion Problems, page 3
               The solution is unique:
               1 3 2 5 4
               2 5 4 1 3
               4 1 3 2 5
               5 4 1 3 2
               3 2 5 4 1
            */
            n = 5;
            int[][] matrixX = {{1, 0, 0, 0, 4},
                               {0, 5, 0, 0, 0},
                               {4, 0, 0, 2, 0},
                               {0, 4, 0, 0, 0},
                               {0, 0, 5, 0, 1}};

            matrix = matrixX;
        }
 

        //
        // create variable matrix
        //
        x = new Var[n][n];
        for(int i = 0; i < n; i++) {
            for(int j = 0; j < n; j++) {
                x[i][j] = p.variable("x-" + i + "-" + j, 1, n);
                if (matrix[i][j] > 0) {
                    p.post(x[i][j], "=", matrix[i][j]);
                }
            }
        }

        //
        // and do a quasigroup completion
        //
        k_all_different(x, n);


    }
    
    
    public void solve() {
        //
        // search
        //
        Solver solver = p.getSolver();
        SearchStrategy strategy = solver.getSearchStrategy();

        Var[] vars_flatten = new Var[n*n];
        for(int i = 0; i < n; i++) {
            for(int j = 0; j < n; j++) {
                vars_flatten[i*n+j] = x[i][j];
            }
        }
        strategy.setVars(vars_flatten);

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
        strategy.setValueSelectorType(ValueSelectorType.MIN);
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
        while (iter.hasNext()) {
            num_sols++;
            Solution s = iter.next();
            // s.log();

            System.out.println("Solution #" + num_sols);
            for(int i = 0; i < n; i++) {
                for(int j = 0; j < n; j++) {
                    System.out.print(s.getValue("x-"+i+"-"+j) + " ");
                }
                System.out.println();
            }
            System.out.println();


        }

        solver.logStats();
    }

    /**
     *
     * Reads a Quasigroup completion file.
     * File format:
     *  # a comment which is ignored
     *  % a comment which also is ignored
     *  number of rows (n)
     *  <
     *    row number of space separated entries
     *  >
     * 
     * "." or "0" means unknown, integer 1..n means known value
     * 
     * Example
     *   5
     *    1 . . . 4
     *   . 5 . . .
     *   4 . . 2 .
     *   . 4 . . .
     *   . . 5 . 1
     *
     */
    public void readFile(String file) {

        System.out.println("readFile(" + file + ")");
        int lineCount = 0;
        
        try {

            BufferedReader inr = new BufferedReader(new FileReader(file));
            String str;
            while ((str = inr.readLine()) != null && str.length() > 0) {

                str = str.trim();

                // ignore comments
                if(str.startsWith("#") || str.startsWith("%")) {
                    continue;
                }

                System.out.println(str);
                if (lineCount == 0) {
                    n = Integer.parseInt(str); // number of rows
                    matrix = new int[n][n];
                } else {
                    // the problem matrix
                    String row[] = str.split(" ");
                    for(int i = 0; i < n; i++) {
                        String s = row[i];
                        if (s.equals(".")) {
                            matrix[lineCount-1][i] = 0;
                        } else {
                            matrix[lineCount-1][i] = Integer.parseInt(s);
                        }
                    }
                }

                lineCount++;

            } // end while
            System.out.println();
            inr.close();

        } catch (IOException e) {
            System.out.println(e);
        }

    } // end readFile
    

    //
    // prints a variable matrix
    //
    void printMatrix(Var[][] y, int rows, int cols) {

        for(int i = 0; i < rows; i++) {
            for(int j = 0; j < cols; j++) {
                System.out.print(y[i][j].getValue()+ " ");
            }
            System.out.println();
        }
        System.out.println();

    } // end printMatrix



}
