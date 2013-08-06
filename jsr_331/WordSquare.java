package org.jcp.jsr331.hakan;

/**
 *
 * Word square problem in JSR-331.
 *
 *  From http://en.wikipedia.org/wiki/Word_square
 *  """
 *  A word square is a special case of acrostic. It consists of a set of words,
 *  all having the same number of letters as the total number of words (the
 *  "order" of the square); when the words are written out in a square grid
 *  horizontally, the same set of words can be read vertically.
 *  """
 *
 *  Compare with the following models:
 * - Choco: http://www.hakank.org/choco/WordSquare.java
 * - Comet: http://www.hakank.org/comet/word_square.co
 * - Gecode: http://www.hakank.org/gecode/word_square.cpp
 * - Google CP Solver: http://www.hakank.org/google_or_tools/word_square.py
 * - JaCoP: http://www.hakank.org/JaCoP/WordSquare.java
 * - MiniZinc: http://www.hakank.org/minizinc/word_square.mzn
 * - Zinc: http://www.hakank.org/minizinc/word_square.zinc
 * - Gecode: http://www.hakank.org/gecode/word_square2.cpp
 *
 * Model by Hakan Kjellerstrand (hakank@bonetmail.com)
 * Also see http://www.hakank.org/jsr_331/
 *
 */

import javax.constraints.*;

import java.io.*;
import java.util.*;
import java.text.*;

public class WordSquare  {

    Var E[];
    Problem p = ProblemFactory.newProblem("WordSquare");

    // defaults
    static int word_len = 5;
    static int num_solutions = 10;
    static String dict_file = "data/words.txt"; 

    static ArrayList<String> dict = new ArrayList<String>(); // the word from the dictionary

    // main
    public static void main(String[] args) {

        if (args.length > 0) {
            word_len = Integer.parseInt(args[0]);
        }

        if (args.length > 1) {
            num_solutions = Integer.parseInt(args[1]);
        }

        if (args.length > 2) {
            dict_file = args[2];
        }

        // Read the dictionary
        System.out.println("Reading the dictionary " + dict_file);
        try {
            BufferedReader inr = new BufferedReader(new FileReader(dict_file));
            String str;
            while ((str = inr.readLine()) != null) {
                str = str.trim();
                if (str.length() == word_len) {
                    if (str.matches("^[a-z]+$")) {
                        dict.add(str);
                    }
                }
            }

            inr.close();
           
            WordSquare p = new WordSquare();
            p.define();
            p.solve();

        } catch (Exception e) {
            System.out.println(e);
        }

    }
    

    // Problem definition    
    public void define() {

        //
        // place all words from the dictionary in the words matrix
        // 
        // We use the same trickery as in the JaCoP mode: 
        // using the _transpose_ of this word matrix in the 
        // Element constraints below.

        int dict_size = dict.size();
        System.out.println("Number of words: " + dict_size);
        int[][] words = new int[word_len][dict_size];
        Iterator it = dict.iterator();
        int k = 0;
        while (it.hasNext()) {
            String str = (String) it.next();
            for(int j = 0; j < word_len; j++) {
                words[j][k] = str.charAt(j);
            }
            k++;
        }
        

        //
        // The variables: which word is selected
        //
        E = p.variableArray("E", 0, dict_size, word_len);

        // redundant constraint, but makes it faster
        p.postAllDifferent(E);

        // 
        // The overlappings (crossings)
        // 
        for(int i = 0; i < word_len ; i++) {
            for(int j = 0; j < word_len ; j++) {
                // Comet: words[E[i], j] ==  words[E[j],i]

                Var tmp = p.variable("tmp" + i + " " + j, 0, dict_size);
                // Var tmp = new javax.constraints.impl.Var(E[0].getProblem(), "tmp" + i + " " + j, 0, dict_size);
                p.postElement(words[j], E[i], "=", tmp);
                p.postElement(words[i], E[j], "=", tmp);

            }
        }

    }
    
    
    public void solve() {
        //
        // search
        //
        Solver solver = p.getSolver();
        SearchStrategy strategy = solver.getSearchStrategy();
        strategy.setVars(E);

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
        
        // find one solution
        Solution solution = solver.findSolution(ProblemState.RESTORE);
        if (solution != null) {
        for(int i = 0; i < word_len; i++) {
            System.out.println(dict.get(solution.getValue("E-"+i)));
        }
        }
        else
        	System.out.println("No solutions found");
 
        
        // find num_solutions
        int num_sols = 0;
        SolutionIterator iter = solver.solutionIterator();
        while (iter.hasNext()) {
            num_sols++;
            Solution s = iter.next();

            System.out.println("Words: " + num_sols);
            for(int i = 0; i < word_len; i++) {
                System.out.println(dict.get(s.getValue("E-"+i)));
            }
            System.out.println();

            //s.log();

            if (num_solutions > 0 && num_sols >= num_solutions) {
                break;
            }

        }

        solver.logStats();
    }

}
