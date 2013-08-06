/**
  *
  * Global constraint alldifferent_except_0 (decomposition) in Choco.
  *
  *
  * From Global Constraint Catalog
  * http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_except_0.html
  * """
  * Enforce all variables of the collection VARIABLES to take distinct values, 
  * except those variables that are assigned to 0.
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
import solver.constraints.nary.cnf.Literal;
import solver.constraints.nary.cnf.Node;
import solver.constraints.nary.cnf.Node.*;
import solver.constraints.nary.cnf.ALogicTree;
import solver.search.strategy.IntStrategyFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import util.ESat;

public class AllDifferentExcept0 extends AbstractProblem {

  @Option(name = "-n", usage = "n, size of array (default 6).", required = false)
  int n = 4;

  @Option(name = "-m", usage = "Max value (default n).", required = false)
  int m = 0;

  IntVar[] x;
  
  //
  // decomposition of alldifferent except 0
  //
  public void allDifferentExcept0(IntVar[] v) {
    
    allDifferentExceptC(v, 0);
    
  }
  
  //
  // slightly more general: alldifferent except c
  //
  public void allDifferentExceptC(IntVar[] v, int c) {
    int len = v.length;
    
    for(int i = 0; i < v.length; i++) {
      for(int j = i+1; j < v.length; j++) {

        // if v[i] != c && v[j] != c -> v[i] != v[j]
        // Alternative approach
        // a[i] != c) * (a[j] != c) <= (a[i] != a[j])

        /*
          // Choco 2 code: Boring but acceptable.
          m.addConstraint(ifThenElse(
                                     and(
                                         gt(v[i], c), 
                                         gt(v[j], c)
                                        ), 
                                     neq(v[i], v[j]),
                                     TRUE
                                     )
                                );
        */

        // 2013-04-15/hakank: How do I a similar construct in Choco3?

        // 
        // This works but is way too complex to be natural...
        //
        BoolVar[] b = VariableFactory.boolArray("b_"+i+"_"+j, 3, solver);

        // Note: As stated in IntConstraintFactory.java, we have to state both ways 
        // of implies, i.e. 
        //        B => C
        //        not(B) => "not"(C)
        //
        solver.post(IntConstraintFactory.implies(
                                                 b[0],
                                                 (IntConstraintFactory.arithm(v[i], "!=", c))));
        solver.post(IntConstraintFactory.implies(
                                                 VariableFactory.not(b[0]),
                                                 (IntConstraintFactory.arithm(v[i], "=", c))));

        solver.post(IntConstraintFactory.implies(
                                                 b[1],
                                                 (IntConstraintFactory.arithm(v[j], "!=", c))));
        solver.post(IntConstraintFactory.implies(
                                                 VariableFactory.not(b[1]),
                                                 (IntConstraintFactory.arithm(v[j], "=", c))));

        solver.post(IntConstraintFactory.implies(
                                                 b[2],
                                                 (IntConstraintFactory.arithm(v[i], "!=", v[j]))));
        solver.post(IntConstraintFactory.implies(
                                                 VariableFactory.not(b[2]),
                                                 (IntConstraintFactory.arithm(v[i], "=", v[j]))));

        // b[0] * b[1] <= b[2]
        /*
        IntVar b01 = VariableFactory.enumerated("b2", 0, 1, solver);
        solver.post(IntConstraintFactory.times(b[0], b[1], b01));
        solver.post(IntConstraintFactory.arithm(b01, "<=", b[2]));
        */

        // This works as well (and is a little more natural the variant above):
        // if b[0] && b[1] -> b[2]
        ALogicTree t = Node.implies(
                                    Node.and(Literal.pos(b[0]), Literal.pos(b[1])), 
                                    Literal.pos(b[2])
                                    );
        solver.post(IntConstraintFactory.clauses(t, solver));

      }
    }
  }


  public void increasing(IntVar[] v) {
    for(int j = 1; j < v.length; j++) {
      solver.post(IntConstraintFactory.arithm(v[j],">=", v[j-1]));
    }
  }

  @Override
  public void createSolver() {
    solver = new Solver("AllDifferentExcept0(" + n + ")");
  }
  

  @Override
  public void buildModel() {    

    // Check m
    if (m == 0) {
      m = n;
    }

    x = VariableFactory.enumeratedArray("x", n, 0, m, solver);
    
    increasing(x);
    allDifferentExcept0(x);
    // allDifferentExceptC(x, 0);

  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.inputOrder_InDomainMin(x));
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
    new AllDifferentExcept0().execute(args);
  }


} // end class
 
