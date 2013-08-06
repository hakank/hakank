package org.jcp.jsr331.hakan;

/**
 *
 * Cross word problem in JSR-331.
 *
 *
 *
 * 1   2   3   4   5
 *   +---+---+---+---+---+       Given the list of words:
 * 1 | 1 |   | 2 |   | 3 |             AFT     LASER
 *   +---+---+---+---+---+             ALE     LEE
 * 2 | # | # |   | # |   |             EEL     LINE
 *   +---+---+---+---+---+             HEEL    SAILS
 * 3 | # | 4 |   | 5 |   |             HIKE    SHEET
 *   +---+---+---+---+---+             HOSES   STEER
 * 4 | 6 | # | 7 |   |   |             KEEL    TIE
 *   +---+---+---+---+---+             KNOT
 * 5 | 8 |   |   |   |   |
 *   +---+---+---+---+---+       
 * 6 |   | # | # |   | # |       The numbers 1,2,3,4,5,6,7,8 in the crossword
 *   +---+---+---+---+---+       puzzle correspond to the words 
 *
 *
 * Compare with the followings models:
 * - Choco: http://www.hakank.org/choco/CrossWord.java
 * - Comet: http://www.hakank.org/comet/crossword.co
 * - ECLiPSE: http://www.hakank.org/eclipse/crossword2.ecl
 * - Gecode/R: http://www.hakank.org/gecode_r/crossword.rb
 * - Gecode: http://www.hakank.org/gecode/crossword.cpp
 * - Gecode: http://www.hakank.org/gecode/crossword2.cpp
 * - Google CP Solver: http://www.hakank.org/google_or_tools/crossword2.py
 * - JaCoP: http://www.hakank.org/JaCoP/CrossWord.java
 * - MiniZinc: http://www.hakank.org/minizinc/crossword.mzn
 * - MiniZinc: http://www.hakank.org/minizinc/crossword2.mzn
 * - SICStus: http://www.hakank.org/sicstus/crossword2.pl
 * - Zinc: http://www.hakank.org/minizinc/crossword2.zinc
 *
 * Model by Hakan Kjellerstrand (hakank@bonetmail.com)
 * Also see http://www.hakank.org/jsr_331/
 *
 */
import javax.constraints.*;

public class CrossWord  {

    String letters = "abcdefghijklmnopqrstuvwxyz";
    String[] letters_array;
    int word_len  = 5;
    int[][] words;

    Var E[];
    int N = 5;

    Problem problem = ProblemFactory.newProblem("CrossWord");


    // main
    public static void main(String[] args) {

        CrossWord pp = new CrossWord();
        pp.define();
        pp.solve();

    }
    

    // Problem definition    
    public void define() {


        letters.trim();
        letters_array = letters.split("");

        // the letters
        int a = 1; int b = 2; int c = 3; int d = 4; 
        int e = 5; int f = 6; int g = 7; int h = 8; 
        int i = 9; int j =  10; int k =  11; int l =  12; 

        int m =  13; int n =  14; int o =  15; int p =  16; 
        int q =  17; int r =  18; int s =  19; int t =  20; 
        int u =  21; int v =  22; int w =  23; int x =  24; 
        int y =  25; int z =  26; 
        
        int zero = 0; 
        
        //
        // these are the words to use
        //
        int num_words = 15;
        int max_word_length = 5;
        int[][] _words = {{h, o, s, e, s},        //  HOSES
                      {l, a, s, e, r},        //  LASER
                      {s, a, i, l, s},        //  SAILS
                      {s, h, e, e, t},        //  SHEET
                      {s, t, e, e, r},        //  STEER
                      {h, e, e, l, zero},     //  HEEL
                      {h, i, k, e, zero},     //  HIKE
                      {k, e, e, l, zero},     //  KEEL
                      {k, n, o, t, zero},     //  KNOT
                      {l, i, n, e, zero},     //  LINE
                      {a, f, t, zero, zero},  //  AFT
                      {a, l, e, zero, zero},  //  ALE
                      {e, e, l, zero, zero},  //  EEL
                      {l, e, e, zero, zero},  //  LEE
                      {t, i, e, zero, zero}}; //  TIE

        words = _words;

        //
        // Some trickery: 
        // We use the _transpose_ of the AX matrix in the 
        // Element constraints below.
        //
        int[][] words_t = new int[word_len][num_words];
        for(int I = 0; I < word_len; I++) {
            for(int J = 0; J < num_words; J++) {
                words_t[I][J] = words[J][I];
            }
        }

        int num_positions = 8;
        E = problem.variableArray("E", 0, 15, num_positions);


        // 
        // The overlapping (crossings)
        // 
        int num_overlapping = 12;
        int[][] overlapping = {{0,2,   1,0},   //  s   AX[E[0], 2] = AX[E[1], 0]
                               {0,4,   2,0},   //  s   etc
                               
                               {3,1,   1,2},   //  i
                               {3,2,   4,0},   //  k
                               {3,3,   2,2},   //  e
                               
                               {6,0,   1,3},   //  l
                               {6,1,   4,1},   //  e
                               {6,2,   2,3},   //  e
                               
                               {7,0,   5,1},   //  l
                               {7,2,   1,4},   //  s
                               {7,3,   4,2},   //  e
                               {7,4,   2,4}};  //  r


        for (int I = 0; I < num_overlapping; I++) {
            Var tmp = problem.variable("TMP" + I, 0, num_words*word_len);
            problem.postElement(words_t[overlapping[I][1]], E[overlapping[I][0]], "=", tmp);
            problem.postElement(words_t[overlapping[I][3]], E[overlapping[I][2]], "=", tmp);
        }

    }
    
    
    public void solve() {
        //
        // search
        //
        Solver solver = problem.getSolver();
        SearchStrategy strategy = solver.getSearchStrategy();
        strategy.setVars(E);

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

            for(int I = 0; I < E.length; I++) {
                int e_val = E[I].getValue();
                System.out.print("E" + I + ": " +  e_val + "\t");
                for(int J = 0; J < word_len; J++) {
                    System.out.print(letters_array[words[e_val][J]]);
                }
                System.out.println();
            }
            System.out.println();

            // s.log();

        }

        solver.logStats();
    }

}
