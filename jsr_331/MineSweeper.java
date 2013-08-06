package org.jcp.jsr331.hakan;

/**
 *
 * Minesweeper problem in JSR-331.
 *
 * References:
 * 
 * The first 10 examples are from gecode/examples/minesweeper.cc
 * http://www.gecode.org/gecode-doc-latest/minesweeper_8cc-source.html
 *
 *
 * http://www.janko.at/Raetsel/Minesweeper/index.htm
 *
 * http://en.wikipedia.org/wiki/Minesweeper_(computer_game)
 * 
 * Ian Stewart on Minesweeper: http://www.claymath.org/Popular_Lectures/Minesweeper/
 *
 * Richard Kaye's Minesweeper Pages:
 * http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.htm
 *
 * Some Minesweeper Configurations:
 * http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.pdf
 *
 * Compare witht the following models:
 * - Choco: http://www.hakank.org/choco/MineSweeper.java
 * - Comet: http://www.hakank.org/comet/minesweeper.co
 * - ECLiPSE: http://www.hakank.org/eclipse/minesweeper.ecl
 * - Gecode/R: http://www.hakank.org/gecode_r/minesweeper.rb
 * - Google CP Solver: http://www.hakank.org/google_or_tools/minesweeper.py
 * - JaCoP: http://www.hakank.org/JaCoP/MineSweeper.java
 * - MiniZinc: http://www.hakank.org/minizinc/minesweeper.mzn
 * - SICStus: http://www.hakank.org/sicstus/minesweeper.pl
 * - Tailor/Essence': http://www.hakank.org/tailor/minesweeper.eprime
 * - Zinc: http://www.hakank.org/minizinc/minesweeper.zinc
 *
 * Model by Hakan Kjellerstrand (hakank@bonetmail.com)
 * Also see http://www.hakank.org/jsr_331/
 *
 */

import javax.constraints.*;

import java.io.*;
import java.util.*;
import java.text.*;

public class MineSweeper {

    int r;        // number of rows
    int c;        // number of cols
    int X = -1;  // represents the unknown value in the problem matrix

    int[][] problem; // The problem matrix

    Var[][] game;    // The Var version of the problem matrix.
    Var[][] mines;   // solution matrix: 0..1 where 1 means mine.

  Problem p = ProblemFactory.newProblem("Minesweeper");


    // main
    public static void main(String[] args) {

        MineSweeper pp = new MineSweeper();
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

        if (problem == null) {
           
            //
            // This problem is from Gecode/examples/minesweeper.cc,  problem 1
            // (also as the file minesweeper1.txt)
            //
            r = 8;
            c = 8;
            int problemX[][] = {{X,2,X,2,1,1,X,X},
                                {X,X,4,X,2,X,X,2},
                                {2,X,X,2,X,X,3,X},
                                {2,X,2,2,X,3,X,3},
                                {X,X,1,X,X,X,4,X},
                                {1,X,X,X,2,X,X,3},
                                {X,2,X,2,2,X,3,X},
                                {1,X,1,X,X,1,X,1}};

            /*
            r = 4;
            c = 4;
            int problemX[][] = {{1,X,X,X},
                                {X,X,4,X},
                                {X,X,X,X},
                                {1,X,X,2}};
            */

            problem = problemX;

        }


        //
        // Initialize the constraint variables.
        //
        mines = new Var[r][c];
        game  = new Var[r][c];

        for(int i = 0; i < r; i++) {
            mines[i] = p.variableArray("m-" + i, 0, 1, c);
            game[i] = p.variableArray("g-" + i, -1, 8, c);
        }

        // Add the constraints
        for(int i = 0; i < r; i++) {
            for(int j = 0; j < c; j++) {
                // This is a known value of neighbours
                if (problem[i][j] > X) {

                    // mirroring the problem matrix.
                    p.post(game[i][j], "=", problem[i][j]);

                    // This could not be a mine.
                    p.post(mines[i][j], "=", 0);

                    // Sum the number of neighbours: same as game[i][j].
                    // 
                    // Note: Maybe this could be modelled more elegant
                    // instead of using an ArrayList.
                    ArrayList<Var> lst = new ArrayList<Var>();
                    for(int a = -1; a <= 1; a++) {
                        for(int b = -1; b <= 1; b++) {
                            if (i+a >= 0 && j+b >=  0 &&
                                i+a < r && j+b < c) {
                                lst.add(mines[i+a][j+b]);
                            }
                        }                        
                    }
                    p.post(p.sum(lst.toArray(new Var[1])), "=", game[i][j]);

                } // end if problem[i][j] > X

            } // end for j

        } // end for i

    }
    
    
    public void solve() {
        //
        // search
        //
        Solver solver = p.getSolver();
        SearchStrategy strategy = solver.getSearchStrategy();

        Var[] vars_flatten = new Var[r*c];
        for(int i = 0; i < r; i++) {
            for(int j = 0; j < c; j++) {
                vars_flatten[i*c+j] = mines[i][j];
            }
        }
        strategy.setVars(vars_flatten);

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
            // s.log();

            System.out.println("Solution #" + num_sols);
            System.out.println("Mines:");
            for(int i = 0; i < r; i++) {
                for(int j = 0; j < c; j++) {
                    System.out.print(s.getValue("m-"+i+"-"+j) + " ");
                }
                System.out.println();
            }
            System.out.println();


        }

        solver.logStats();
    }


    /**
     *
     * Reads a minesweeper file.
     * File format:
     *  # a comment which is ignored
     *  % a comment which also is ignored
     *  number of rows
     *  number of columns
     *  <
     *    row number of neighbours lines...
     *  >
     * 
     * 0..8 means number of neighbours, "." mean unknown (may be a mine)
     * 
     * Example (from minesweeper0.txt)
     * # Problem from Gecode/examples/minesweeper.cc  problem 0
     * 6
     * 6
     * ..2.3.
     * 2.....
     * ..24.3
     * 1.34..
     * .....3
     * .3.3..
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
                    r = Integer.parseInt(str); // number of rows
                } else if (lineCount == 1) {
                    c = Integer.parseInt(str); // number of columns
                    problem = new int[r][c];
                } else {
                    // the problem matrix
                    String row[] = str.split("");
                    for(int j = 1; j <= c; j++) {
                        String s = row[j];
                        if (s.equals(".")) {
                            problem[lineCount-2][j-1] = -1;
                        } else {
                            problem[lineCount-2][j-1] = Integer.parseInt(s);
                        }
                    }
                }

                lineCount++;

            } // end while

            inr.close();

        } catch (IOException e) {
            System.out.println(e);
        }

    } // end readFile


}
