/**
 *
 * Minesweeper problem in Choco3.
 *
 * This is a port of my MiniZinc and JaCoP models
 * http://www.hakank.org/minizinc/minesweeper.mzn
 * http://www.hakank.org/JaCoP/MineSweeper.java
 * 
 * which is commented in the (Swedish) blog post
 * "Fler constraint programming-modeller i MiniZinc, t.ex. Minesweeper och Game of Life"
 * http://www.hakank.org/webblogg/archives/001231.html
 *
 * 
 * See also
 *  
 * The first 10 examples are from gecode/examples/minesweeper.cc
 * http://www.gecode.org/gecode-doc-latest/minesweeper_8cc-source.html
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
 *
 *
 * Choco3 model by Hakan Kjellerstrand (hakank@gmail.com)
 * Also see http://www.hakank.org/choco3/
 *
 */


/*
 * Comparison with the MiniZinc model for the minesweeper_kaye_splitter.txt.
 * 
 * There are 131072 solutions.
 *
 * MiniZinc:
 * The Gecode/fz solver MiniZinc reports the following statistics:
 * with the solver annotations: "first_fail", "indomain", "complete".
 *
 *      runtime:       48620
 *      solutions:     131072
 *      propagators:   33
 *      propagations:  13209
 *      failures:      4
 *      clones:        131075
 *      commits:       342933
 *      peak memory:   148 KB
 *
 * Solving and printing one solution takes about 1 second.
 *
 *
 */
import org.kohsuke.args4j.Option;
import org.slf4j.LoggerFactory;
import samples.AbstractProblem;
import solver.ResolutionPolicy;
import solver.Solver;
import solver.constraints.Constraint;
import solver.constraints.IntConstraintFactory;
import solver.search.strategy.IntStrategyFactory;
import solver.search.loop.monitors.SearchMonitorFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import util.ESat;
import util.tools.ArrayUtils;

import java.io.*;
import java.util.*;
import java.text.*;


public class MineSweeper extends AbstractProblem {

  @Option(name = "-file", usage = "instance file.", required = false)
  String file = null; // "minesweeper0.txt";

  int r;        // number of rows
  int c;        // number of cols
  int X = -1;  // represents the unknown value in the problem matrix

  int[][] problem; // The problem matrix
  IntVar[][] game;    // The IntVar version of the problem matrix.
  IntVar[][] mines;   // solution matrix: 0..1 where 1 means mine.


  @Override
  public void buildModel() {

    if (file == null) {

      //
      // This problem is from Gecode/examples/minesweeper.cc,  problem 1
      // (also as the file minesweeper1.txt)
      //
      r = 8;
      c = 8;
      problem = new int[][] {{X,2,X,2,1,1,X,X},
                             {X,X,4,X,2,X,X,2},
                             {2,X,X,2,X,X,3,X},
                             {2,X,2,2,X,3,X,3},
                             {X,X,1,X,X,X,4,X},
                             {1,X,X,X,2,X,X,3},
                             {X,2,X,2,2,X,3,X},
                             {1,X,1,X,X,1,X,1}};

    } else {

      readFile(file);

    }


    //
    // Initialize the constraint variables.
    //
    mines = VariableFactory.enumeratedMatrix("m", r, c, 0, 1, solver);
    game = VariableFactory.enumeratedMatrix("game", r, c, -1, 8, solver);
        

    // Add the constraints
    for(int i = 0; i < r; i++) {
      for(int j = 0; j < c; j++) {

        // This is a known value of neighbours
        if (problem[i][j] > X) {

          // mirroring the problem matrix.
          solver.post(IntConstraintFactory.arithm(game[i][j], "=", problem[i][j]));

          // This could not be a mine.
          solver.post(IntConstraintFactory.arithm(mines[i][j],"=", 0));

          // Sum the number of neighbours: same as game[i][j].
          ArrayList<IntVar> neighbours = new ArrayList<IntVar>();
          for(int a = -1; a <= 1; a++) {
            for(int b = -1; b <= 1; b++) {
              if (i+a >= 0 && j+b >=  0 &&
                  i+a < r && j+b < c) {
                neighbours.add(mines[i+a][j+b]);
              }
            }                        
          }

          solver.post(IntConstraintFactory.sum(neighbours.toArray(new IntVar[1]), game[i][j]));

        } // end if problem[i][j] > X
      }
    }

  }

  @Override
  public void createSolver() {
    solver = new Solver("MineSweeper");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(ArrayUtils.flatten(mines)));
  }

  @Override
  public void solve() {
    // System.out.println(solver); // Solver/model before solve.
    solver.findSolution();
  }

  @Override
  public void prettyOut() {
    String fileStr = file != null ? file : "";

    LoggerFactory.getLogger("bench").info("MineSweeper(" + fileStr + ")");

    StringBuilder st = new StringBuilder();
   
    if (solver.isFeasible() == ESat.TRUE) {
      int sol = 1;
      do {
        st.append("\nSolution # " + sol + "\n");

        for(int i = 0; i < r; i++) {
          for(int j = 0; j < c; j++) {
            st.append(mines[i][j].getValue() + " ");
          }
          st.append("\n");
        }
        st.append("\n");

        sol++;

        } while (solver.nextSolution() == Boolean.TRUE);

    } else {

      st.append("No solutions.\n");
    } 
    
    LoggerFactory.getLogger("bench").info(st.toString());    

  } // end search


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
  private void readFile(String file) {

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


  public static void main(String args[]) {

    new MineSweeper().execute(args);

  } // end main

} // end class

