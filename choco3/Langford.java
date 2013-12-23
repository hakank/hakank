/**
  *
  * Langford's number problem in Choco3.
  *
  * Langford's number problem (CSP lib problem 24)
  * See CSPLib http://www.csplib.org/prob/prob024/
  * 
  * From http://www.cs.st-andrews.ac.uk/~andrea/examples/langford/langford.eprime
  * """
  * Arrange 2 sets of positive integers 1..k to a sequence,
  * such that, following the first occurence of an integer i, 
  * each subsequent occurrence of i, appears i+1 indices later
  * than the last. 
  * For example, for k=4, a solution would be 41312432
  * """
  *
  * From http://legacy.lclark.edu/~miller/langford/MG.html
  * """
  * If n is the number of pairs, the problem has a solution only if n is a 
  * multiple of four or one less than such a multiple. 
  * """
  *
  * Also see: 
  *  - http://legacy.lclark.edu/~miller/langford.html
  *  - http://mathworld.wolfram.com/LangfordsProblem.html
  * 
  * This Choco3 model was created by Hakan Kjellerstrand (hakank@bonetmail.com)
  * Also, see my Choco page: http://www.hakank.org/choco/ 
  *
  */
import org.kohsuke.args4j.Option;
import org.slf4j.LoggerFactory;
import samples.AbstractProblem;
import solver.ResolutionPolicy;
import solver.Solver;
import solver.constraints.Constraint;
import solver.constraints.IntConstraintFactory;
import solver.constraints.IntConstraintFactory.*;
import solver.search.strategy.IntStrategyFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import util.ESat;
import util.tools.ArrayUtils;

import java.util.*;

public class Langford extends AbstractProblem {

  @Option(name = "-k", usage = "size of problem (default 8)", required = false)
  int k = 8;

  @Option(name = "-solutions", usage = "# solutions to show (default 0, show all)", required = false)
  int solutions = 0;


  int p;
 
  IntVar[] position;
  IntVar[] solution;
  IntVar[] all;      // for search


  @Override
  public void createSolver() {
    solver = new Solver("Langford");
  }
  

  @Override
  public void buildModel() {    

    if (!(k % 4 == 0 || (k+1) % 4 == 0)) {
      System.out.println("k must be a multiple of 4 or one less than such a multiple.");
      System.exit(1);
    }

    p = k*2;

    position = VariableFactory.enumeratedArray("position", p, 0, p-1, solver); 
    solution = VariableFactory.enumeratedArray("solution", p, 1, k, solver); 

    // For search
    all = VariableFactory.enumeratedArray("all", p*2, 0, p-1, solver); 


    for(int i = 1; i <= k; i++) {
      //  position[i+k] = position[i] + i+1
      solver.post(IntConstraintFactory.arithm(position[i+k-1], "=", VariableFactory.offset(position[i-1],i+1)));

      IntVar iconst = VariableFactory.fixed(i, solver);
      // solution[position[i]] = i
      // solution[position[k+i]] = i
      solver.post(IntConstraintFactory.element(iconst, solution, position[i-1], 0));
      solver.post(IntConstraintFactory.element(iconst, solution, position[k+i-1], 0));

    }

    // for search
    for(int i = 0; i < p; i++) {
      all[i] = position[i];
      all[p+i] = solution[i];
    }

    solver.post(IntConstraintFactory.alldifferent(position, "BC"));

    // Symmetry breaking
    solver.post(IntConstraintFactory.arithm(solution[0], ">", solution[p-1]));


    // Redundant constraint
    /*
    IntVar two = VariableFactory.fixed(2, solver);
    for(int i = 1; i <= k; i++) {
      solver.post(IntConstraintFactory.count(i,solution,two));
    }
    */

    // Experimental symmetry breaking
    // solver.post(IntConstraintFactory.arithm(solution[0],"=", VariableFactory.fixed(k, solver)));

  }


  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(all));
  }
  
  @Override
  public void solve() {
    solver.findSolution();
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_sol = 0;
      do {
        System.out.print("position: ");
        for (int i = 0; i < p; i++) {
          System.out.print(position[i].getValue() + " ");
        }
        System.out.print("\nsolution: ");
        for (int i = 0; i < p; i++) {
          System.out.print(solution[i].getValue() + " ");
        }
        System.out.println();

        num_sol++;

        if (solutions > 0 && num_sol >= solutions) {
          break;
        }

      } while (solver.nextSolution() == Boolean.TRUE);
      
      System.out.println("\nIt was " + num_sol + " solutions.");
      
    } else {
      System.out.println("No solution.");
    }

  }

  public static void main(String[] args) {

    new Langford().execute(args);

  }


}
 
