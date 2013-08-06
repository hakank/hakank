package org.jcp.jsr331.hakan;

/**
 *
 * Survo puzzle in JSR-331.
 *
 * http://en.wikipedia.org/wiki/Survo_Puzzle
 * """
 * Survo puzzle is a kind of logic puzzle presented (in April 2006) and studied 
 * by Seppo Mustonen. The name of the puzzle is associated to Mustonen's 
 * Survo system which is a general environment for statistical computing and 
 * related areas.
 * 
 * In a Survo puzzle the task is to fill an m * n table by integers 1,2,...,m*n so 
 * that each of these numbers appears only once and their row and column sums are 
 * equal to integers given on the bottom and the right side of the table. 
 * Often some of the integers are given readily in the table in order to 
 * guarantee uniqueness of the solution and/or for making the task easier.
 * """
 * 
 * See also
 * http://www.survo.fi/english/index.html
 * http://www.survo.fi/puzzles/index.html
 *
 * References:
 * - Mustonen, S. (2006b). "On certain cross sum puzzles"
 *   http://www.survo.fi/papers/puzzles.pdf 
 * - Mustonen, S. (2007b). "Enumeration of uniquely solvable open Survo puzzles." 
 *   http://www.survo.fi/papers/enum_survo_puzzles.pdf 
 * - Kimmo Vehkalahti: "Some comments on magic squares and Survo puzzles" 
 *   http://www.helsinki.fi/~kvehkala/Kimmo_Vehkalahti_Windsor.pdf
 *
 * Compare with the following models:
 *  - Choco: http://www.hakank.org/choco/SurvoPuzzle.java
 *  - Comet: http://www.hakank.org/comet/survo_puzzle.co
 *  - ECLiPSE: http://www.hakank.org/eclipse/survo_puzzle.ecl
 *  - Gecode: http://www.hakank.org/gecode/survo_puzzle.cpp
 *  - Gecode/R: http://www.hakank.org/gecode_r/survo_puzzle.rb
 *  - Google CP Solver: http://www.hakank.org/google_or_tools/survo_puzzle.py
 *  - JaCoP: http://www.hakank.org/JaCoP/SurvoPuzzle.java
 *  - MiniZinc: http://www.hakank.org/minizinc/survo_puzzle.mzn
 *  - SICStus: http://www.hakank.org/sicstus/survo_puzzle.pl
 *  - Tailor/Essence': http://www.hakank.org/tailor/survo_puzzle.eprime
 *  - Zinc: http://www.hakank.org/minizinc/survo_puzzle.zinc
 *
 * Model by Hakan Kjellerstrand (hakank@bonetmail.com)
 * Also see http://www.hakank.org/jsr_331/
 *
 */

import javax.constraints.*;

import java.io.*;
import java.util.*;
import java.text.*;

public class SurvoPuzzle {

    int r;          // number of rows
    int c;          // number of column
    int[] rowsums;  // row sums
    int[] colsums;  // col sums
    int[][] matrix; // the clues matrix

    // Var[][] x;      // the solution
    Var[] x_arr;    // x as an array, for alldifferent

    Problem p = ProblemFactory.newProblem("SurvoPuzzle");

    // main
    public static void main(String[] args) {


       String filename = "";
        if (args.length == 1) {
            filename = args[0];
            System.out.println("Using file " + filename);
        }

        SurvoPuzzle pp = new SurvoPuzzle();
        if (filename.length() > 0) {
            pp.readFile(filename);
        }

        pp.define();
        pp.solve();

    }
    

    // Problem definition    
    public void define() {

        if (matrix == null) {

            System.out.println("Using the default problem.");
            
            /* Default problem:
             *
             * http://www.survo.fi/puzzles/280708.txt, the third puzzle
             * Survo puzzle 128/2008 (1700) #364-35846
             */
            int r_tmp = 3;
            int c_tmp = 6;
            int[] rowsums_tmp = {30, 86, 55};
            int[] colsums_tmp = {22, 11, 42, 32, 27, 37};
            int[][] matrix_tmp = {{0, 0,  0, 0, 0, 0},
                                  {0, 0, 18, 0, 0, 0},
                                  {0, 0,   0, 0, 0, 0}};

            r = r_tmp;
            c = c_tmp;
            rowsums = rowsums_tmp;
            colsums = colsums_tmp;
            matrix = matrix_tmp;

        }
        
        //
        // initiate structures and variables
        //
        Var[][] x = new Var[r][c];
        x_arr = new Var[r*c];
        for(int i = 0; i < r; i++) {
            for(int j = 0; j < c; j++) {
                x[i][j] = p.variable("x-" + i + "-" + j, 1, r*c);
                if (matrix[i][j] > 0) {
                    p.post(x[i][j], "=", matrix[i][j]);
                }
                x_arr[c*i+j] = p.variable("xa-" + i + "-" + j, 1, r*c);
                p.post(x_arr[c*i+j], "=", x[i][j]); 
            }
        }
        
        //
        // row sums
        //
        for(int i = 0; i < r; i++) {
            p.post(p.sum(x[i]), "=", rowsums[i]);
        }


        //
        // column sums
        //
        for(int j = 0; j < c; j++) { 
            ArrayList<Var> cols = new ArrayList<Var>();
            for(int i = 0; i < r; i++) {
                cols.add(x[i][j]);
            }
            p.post(p.sum(cols.toArray(new Var[1])), "=", colsums[j]);
        }

        // Alldifferent on the array version.
        p.postAllDifferent(x_arr);

    }
    
    
    public void solve() {
        //
        // search
        //
        Solver solver = p.getSolver();
        SearchStrategy strategy = solver.getSearchStrategy();
        strategy.setVars(x_arr);

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
        int num_sols = 0;
        SolutionIterator iter = solver.solutionIterator();
        while (iter.hasNext()) {
            num_sols++;
            Solution s = iter.next();

            for(int i = 0; i < r; i++) {
                for(int j = 0; j < c; j++) {
                    // System.out.print(s.getValue("xa-"+i+"-"+j) + " ");
                    System.out.format("%3s", Integer.toString(s.getValue("xa-"+i+"-"+j)) + " ");
                }
                System.out.println();
            }
            System.out.println();

            // s.log();
        }

        solver.logStats();
    }


   /**
     *
     * readFile()
     *
     * Reads a Survo puzzle in the following format
     * 
     * % From http://www.survo.fi/puzzles/280708.txt
     * % Survo puzzle 128/2008 (1700) #364-35846
     * A  B  C  D  E  F
     * 1  *  *  *  *  *  * 30
     * 2  *  * 18  *  *  * 86
     * 3  *  *  *  *  *  * 55
     * 22 11 42 32 27 37
     *
     */
    public void readFile(String file) {

        System.out.println("readFile(" + file + ")");

        try {

            BufferedReader inr = new BufferedReader(new FileReader(file));
            String str;
            int lineCount = 0;
            ArrayList<ArrayList<Integer>> MatrixI = new ArrayList<ArrayList<Integer>>();
            while ((str = inr.readLine()) != null && str.length() > 0) {
                
                str = str.trim();
                
                // ignore comments
                // starting with either # or %
                if(str.startsWith("#") || str.startsWith("%")) {
                    continue;
                }

                str = str.replace("_", "");                                
                String row[] = str.split("\\s+");
                System.out.println(str);

                // first line: column names: Ignore but count them
                if (lineCount == 0) {
                    c = row.length;
                    colsums = new int[c];
                } else  {

                    // This is the last line: the column sums
                    if (row.length == c) {
                        colsums = new int[row.length];
                        for(int j = 0; j < row.length; j++) {
                            colsums[j] = Integer.parseInt(row[j]);
                        }
                        System.out.println();
                    } else {
                        // Otherwise:
                        // The problem matrix: index 1 .. row.length-1
                        // The row sums: index row.length
                        ArrayList<Integer> this_row = new ArrayList<Integer>();
                        for(int j = 0; j < row.length; j++) {
                            String s = row[j];
                            if (s.equals("*")) {
                                this_row.add(0);
                            } else {
                                this_row.add(Integer.parseInt(s));
                            }
                        }
                        MatrixI.add(this_row);
                    }
                   
                }
                
                lineCount++;

            } // end while

            inr.close();

            //
            // Now we know everything to be known:
            // Construct the problem matrix and column sums.
            //
            r = MatrixI.size();
            rowsums = new int[r];
            matrix = new int[r][c];
            for(int i = 0; i < r; i++) {
                ArrayList<Integer> this_row = MatrixI.get(i);
                for(int j = 1; j < c + 1 ; j++) {
                    matrix[i][j-1] = this_row.get(j);
                }
                rowsums[i] = this_row.get(c+1);
            }
            
            
        } catch (IOException e) {
            System.out.println(e);
        }
        
    } // end readFile



}
