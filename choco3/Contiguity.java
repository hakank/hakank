/**
  *
  * Decomposition of global constraint contiguity in Choco.
  *
  * This is the contiguity constraint using regular constraint.
  *
  * From Global Constraint Catalogue
  * http://www.emn.fr/x-info/sdemasse/gccat/Cglobal_contiguity.html
  * """
  * Enforce all variables of the VARIABLES collection to be assigned to 0 or 1. 
  * In addition, all variables assigned to value 1 appear contiguously.
  * 
  * Example:
  * (<0,​1,​1,​0>)
  * 
  * The global_contiguity constraint holds since the sequence 0 1 1 0 contains 
  * no more than one group of contiguous 1.
  * """
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
import solver.constraints.nary.automata.FA.FiniteAutomaton;

public class Contiguity extends AbstractProblem {

  @Option(name = "-n", usage = "n, size of array (default 6).", required = false)
  int n = 4;

  IntVar[] x;
  
  //
  // Decomposition of contiguity
  //
  public void contiguity(IntVar[] v) {

    int len = v.length;
    
    FiniteAutomaton auto = new FiniteAutomaton("0*1*0*");
    solver.post(IntConstraintFactory.regular(v, auto));

  }

  @Override
  public void createSolver() {
    solver = new Solver("Contiguity(" + n + ")");
  }
  

  @Override
  public void buildModel() {    

    x = VariableFactory.boolArray("x", n, solver);
    
    contiguity(x);

  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.presetI(x));
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
        System.out.print("x: ");
        for (int i = 0; i < n; i++) {
          System.out.print(x[i].getValue() + " ");
          }
        System.out.println();
        num_sol++;
      } while (solver.nextSolution() == Boolean.TRUE);
      
      System.out.println("\nIt was " + num_sol + " solutions.");
      
    } else {
      System.out.println("No solution.");
    }

  }

  public static void main(String[] args) {
    new Contiguity().execute(args);
  }


} // end class
 
