package org.jcp.jsr331.hakan;

/**
 *
 * Kakuro grid puzzle in JSR-331.
 *
 * http://en.wikipedia.org/wiki/Kakuro
 * """
 * The object of the puzzle is to insert a digit from 1 to 9 inclusive 
 * into each white cell such that the sum of the numbers in each entry 
 * matches the clue associated with it and that no digit is duplicated in 
 * any entry. It is that lack of duplication that makes creating Kakuro 
 * puzzles with unique solutions possible, and which means solving a Kakuro 
 * puzzle involves investigating combinations more, compared to Sudoku in 
 * which the focus is on permutations. There is an unwritten rule for 
 * making Kakuro puzzles that each clue must have at least two numbers 
 * that add up to it. This is because including one number is mathematically 
 * trivial when solving Kakuro puzzles; one can simply disregard the 
 * number entirely and subtract it from the clue it indicates.
 * """
 *
 * This model solves the problem at the Wikipedia page. 
 * For a larger picture, see
 * http://en.wikipedia.org/wiki/File:Kakuro_black_box.svg
 *
 * The solution:
 *   9 7 0 0 8 7 9
 *   8 9 0 8 9 5 7
 *   6 8 5 9 7 0 0
 *   0 6 1 0 2 6 0
 *   0 0 4 6 1 3 2
 *   8 9 3 1 0 1 4
 *   3 1 2 0 0 2 1
 *
 * or rather
 *
 *   9 7 _ _ 8 7 9
 *   8 9 _ 8 9 5 7
 *   6 8 5 9 7 _ _
 *   _ 6 1 _ 2 6 _
 *   _ _ 4 6 1 3 2
 *   8 9 3 1 _ 1 4
 *   3 1 2 _ _ 2 1
 *
 *
 * Compare with the Minizinc model
 * http://www.hakank.org/minizinc/kakuro2.mzn
 *
 *
 * Model by Hakan Kjellerstrand (hakank@bonetmail.com)
 * Also see http://www.hakank.org/jsr_331/
 *
 */

import javax.constraints.*;

import java.util.*;


public class Kakuro2  {
  
  Problem p = ProblemFactory.newProblem("Kakuro2");
  int n;
  Var[][] x;


  // main
  public static void main(String[] args) {
           
    Kakuro2 p = new Kakuro2();
    p.define();
    p.solve();

  }

  public void handle_hints(int[][] hints, int[] sums, int offset) {

    for(int h = 0; h < sums.length; h++) {
      ArrayList<Var> tmp = new ArrayList<Var>();
      for(int i = 0; i < n; i++) {
        for(int j = 0; j < n; j++) {
          if (hints[i][j] == h+1+offset) {
            tmp.add(x[i][j]);
          }
        }
      }

      // convert to Var[]
      int len = tmp.size();
      Var[] v = new Var[len];
      for(int i = 0; i < len; i++) {
        v[i] = tmp.get(i);
      }

      p.post(v,"=", sums[h]);
      p.postAllDifferent(v);

    }

  }

  // Problem definition    
  public void define() {
    
    // 
    // data
    // 
    n = 7;

    int B = -1; // Blank
  
    int num_row_hints = 12;
    int num_col_hints = 12;

    // The numbers are the "hint segment" a hint belongs to.
    // The blanks (B) are also in these grids.

    // Note: The segments are 1-based.
    int[][] hints_row = {
      // 1  2  3  4  5  6  7
      { 1, 1, B, B, 2, 2, 2}, // 1
      { 3, 3, B, 4, 4, 4, 4}, // 2
      { 5, 5, 5, 5, 5, B, B}, // 3
      { B, 6, 6, B, 7, 7, B}, // 4
      { B, B, 8, 8, 8, 8, 8}, // 5
      { 9, 9, 9, 9, B,10,10}, // 6
      {11,11,11, B, B,12,12}}; // 7


    // The column hints
    int[][] hints_col = {
      // 1  2  3  4  5  6  7
      {13,15, B, B,20,21,23}, // 1
      {13,15, B,18,20,21,23}, // 2
      {13,15,17,18,20, B, B}, // 3
      { B,15,17, B,20,22, B}, // 4
      { B, B,17,19,20,22,24}, // 5
      {14,16,17,19, B,22,24}, // 6
      {14,16,17, B, B,22,24}}; // 7


    // sums for row hints
    int[] row_hint_sums = { 16, 24, 17, 29, 35, 7, 8, 16, 21, 5, 6, 3};

    // sums for column hints
    int[] col_hint_sums = { 23, 11, 30, 10, 15, 17, 7, 27, 12, 12, 16, 7};


    //
    // decision variable
    //
    x = new Var[n][n];
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
        String iName = "x" + "[" + i + "," + j	+ "]";
        x[i][j] = p.variable(iName, 0, 9); 
      }
    }

    //
    // constraints
    //

    // handle the blanks
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
        if (hints_row[i][j] == B) {
          p.post(x[i][j], "=", 0);
        } else {
          p.post(x[i][j], ">", 0);
        }
      }
    }

    handle_hints(hints_row, row_hint_sums, 0);
    handle_hints(hints_col, col_hint_sums, num_row_hints);

  }
    
    
  public void solve() {
    //
    // search
    //
    Solver solver = p.getSolver();
    SearchStrategy strategy = solver.getSearchStrategy();
       
    //
    // tracing
    //
    // solver.traceExecution(true);

    //
    // solve
    //  

    // find all solutions
    int num_sols = 0;
    SolutionIterator iter = solver.solutionIterator();
    while (iter.hasNext()) {
      num_sols++;
      Solution s = iter.next();

      for(int i = 0; i < n; i++) {
        for(int j = 0; j < n; j++) {
          int v = s.getValue("x["+i+","+j+"]");
          if (v == 0) {
            System.out.print("_ ");
          } else {
            System.out.print(v + " ");
          }
        }
        System.out.println();
      }
      System.out.println();

    }

    solver.logStats();
  }

}
