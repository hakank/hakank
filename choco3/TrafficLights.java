/**
  *
  * Traffic lights problem (CSPLib #16) in Choco3.
  *
  * CSPLib problem 16
  * http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob016/index.html
  * """
  * Specification:
  * Consider a four way traffic junction with eight traffic lights. Four of the traffic
  * lights are for the vehicles and can be represented by the variables V1 to V4 with domains
  * {r,ry,g,y} (for red, red-yellow, green and yellow). The other four traffic lights are
  * for the pedestrians and can be represented by the variables P1 to P4 with domains {r,g}.
  *
  *  The constraints on these variables can be modelled by quaternary constraints on
  *  (Vi, Pi, Vj, Pj ) for 1<=i<=4, j=(1+i)mod 4 which allow just the tuples
  * {(r,r,g,g), (ry,r,y,r), (g,g,r,r), (y,r,ry,r)}.
  
  *  It would be interesting to consider other types of junction (e.g. five roads
  *  intersecting) as well as modelling the evolution over time of the traffic light sequence.
  *  ...
  *  
  *  Results
  * Only 2^2 out of the 2^12 possible assignments are solutions.
  *
  *  (V1,P1,V2,P2,V3,P3,V4,P4) =
  *  {(r,r,g,g,r,r,g,g), (ry,r,y,r,ry,r,y,r), (g,g,r,r,g,g,r,r), (y,r,ry,r,y,r,ry,r)}
  *  [(1,1,3,3,1,1,3,3), ( 2,1,4,1, 2,1,4,1), (3,3,1,1,3,3,1,1), (4,1, 2,1,4,1, 2,1)}
  *  The problem has relative few constraints, but each is very
  *  tight. Local propagation appears to be rather ineffective on this
  *  problem.  
  """
  *
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
import solver.constraints.propagators.extension.nary.IterTuplesTable;
import solver.constraints.propagators.extension.nary.LargeRelation;
import solver.search.strategy.IntStrategyFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import util.ESat;
import util.tools.ArrayUtils;

import java.util.*;

public class TrafficLights extends AbstractProblem {

  int n = 4;

  int r  = 0;
  int ry = 1;
  int g  = 2;
  int y  = 3;

  String[] lights = {"r", "ry", "g", "y"};

  // Allowed combinations
  int[][] allowed = {{r,r,g,g},
                     {ry,r,y,r},
                     {g,g,r,r},
                     {y,r,ry,r}};

  IntVar[] V;
  IntVar[] P;

  @Override
  public void createSolver() {
    solver = new Solver("TrafficLights");
  }
  

  @Override
  public void buildModel() {    

    V = VariableFactory.enumeratedArray("V", n, 0, n-1, solver);    
    P = VariableFactory.enumeratedArray("P", n, 0, n-1, solver);    

    // How do i use IntConstraintFacory.table(), or rather what is LargeRelation?
    // The few examples are not very helpful!
    // LargeRelation relation = new IterTuplesTable(allowed, offsets, sizes);
    for(int i = 0; i <n; i++) {
      int j = (1+i) % n;
      // IntVar[] t = new IntVar[] {V[i],P[i],V[j],P[j]};
      // List<IntVar> t2 = Arrays.asList(t);
      // solver.post(IntConstraintFactory.table(t2, relation,"FC"));


      // Simulating a poor man's table constraint
      int[][] transposed = ArrayUtils.transpose(allowed);
      IntVar k = VariableFactory.enumerated("k_"+i, 0, n-1, solver);     
      solver.post(IntConstraintFactory.element(V[i],transposed[0],k,0,"detect"));
      solver.post(IntConstraintFactory.element(P[i],transposed[1],k,0,"detect"));
      solver.post(IntConstraintFactory.element(V[j],transposed[2],k,0,"detect"));
      solver.post(IntConstraintFactory.element(P[j],transposed[3],k,0,"detect"));

    }

  }


  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(ArrayUtils.append(V,P)));
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
        System.out.print("V: ");
        for (int i = 0; i < n; i++) {
          System.out.print(V[i].getValue() + " ");
        }
        System.out.print("\nP: ");
        for (int i = 0; i < n; i++) {
          System.out.print(P[i].getValue() + " ");
        }
        System.out.print("\nLights: ");

        for (int i = 0; i < n; i++) {
          System.out.print(lights[V[i].getValue()] + " " + lights[P[i].getValue()] + " ");
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

    new TrafficLights().execute(args);

  }


}
 
