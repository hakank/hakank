/**
  *
  * Calculs d'enfer puzzle in Choco3.
  *
  * Problem from Jianyang Zhou "The Manual of NCL version 1.2", page 33
  * http://citeseer.ist.psu.edu/161721.html
  * 
  * The solution is the manual is:
  * """
  * a = -16, b = -14, c = -13, d = -12, e = -10,
  * f = 4, g = 13, h = -1, i = -3, j = -11, k = -9,
  * l = 16, m = -8, n = 11, o = 0, p = -6, q = -4,
  * r = 15, s = 2, t = 9, u = -15, v = 14, w = -7,
  * x = 7, y = -2, z = -5.
  *
  * max_{#1\in [1,26]}{|x_{#1}|} minimized to 16
  * """
  *
  * Also, see the discussion of the Z model:
  * http://www.comp.rgu.ac.uk/staff/ha/ZCSP/additional_problems/calculs_enfer/calculs_enfer.ps
  * (which shows the same solution).
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
import solver.search.strategy.IntStrategyFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import util.ESat;
import util.tools.ArrayUtils;

import java.util.*;

public class CalculsDEnfer extends AbstractProblem {

  int N = 26; 
 
  IntVar[] A;
  IntVar[] AAbs;
  IntVar AMax;


  @Override
  public void createSolver() {
    solver = new Solver("CalculsDEnfer");
  }
  

  @Override
  public void buildModel() {    

    int lb = -100;
    int ub = 100;

    IntVar a = VariableFactory.enumerated("a", lb, ub, solver);
    IntVar b = VariableFactory.enumerated("b", lb, ub, solver);
    IntVar c = VariableFactory.enumerated("c", lb, ub, solver);
    IntVar d = VariableFactory.enumerated("d", lb, ub, solver);
    IntVar e = VariableFactory.enumerated("e", lb, ub, solver);
    IntVar f = VariableFactory.enumerated("f", lb, ub, solver);
    IntVar g = VariableFactory.enumerated("g", lb, ub, solver);
    IntVar h = VariableFactory.enumerated("h", lb, ub, solver);
    IntVar i = VariableFactory.enumerated("i", lb, ub, solver);
    IntVar j = VariableFactory.enumerated("j", lb, ub, solver);
    IntVar k = VariableFactory.enumerated("k", lb, ub, solver);
    IntVar l = VariableFactory.enumerated("l", lb, ub, solver);
    IntVar m = VariableFactory.enumerated("m", lb, ub, solver);
    IntVar n = VariableFactory.enumerated("n", lb, ub, solver);
    IntVar o = VariableFactory.enumerated("o", lb, ub, solver);
    IntVar p = VariableFactory.enumerated("p", lb, ub, solver);
    IntVar q = VariableFactory.enumerated("q", lb, ub, solver);
    IntVar r = VariableFactory.enumerated("r", lb, ub, solver);
    IntVar s = VariableFactory.enumerated("s", lb, ub, solver);
    IntVar t = VariableFactory.enumerated("t", lb, ub, solver);
    IntVar u = VariableFactory.enumerated("u", lb, ub, solver);
    IntVar v = VariableFactory.enumerated("v", lb, ub, solver);
    IntVar w = VariableFactory.enumerated("w", lb, ub, solver);
    IntVar x = VariableFactory.enumerated("x", lb, ub, solver);
    IntVar y = VariableFactory.enumerated("y", lb, ub, solver);
    IntVar z = VariableFactory.enumerated("z", lb, ub, solver);
    A = new IntVar[] {a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z};

    
    AAbs = VariableFactory.enumeratedArray("AAbs", N, 0, ub, solver); 
    AMax = VariableFactory.enumerated("AMax", 0, ub, solver);

    for (int I = 0; I < N; I++) {
      System.out.println("I: " + I);
      AAbs[I] = VariableFactory.abs(A[I]);
    }

    solver.post(IntConstraintFactory.maximum(AMax, AAbs));
    solver.post(IntConstraintFactory.alldifferent(A, "BC"));

    solver.post(IntConstraintFactory.sum(new IntVar[] {z,e,r,o}, VariableFactory.fixed(0, solver)));
    solver.post(IntConstraintFactory.sum(new IntVar[] {o,n,e}, VariableFactory.fixed(1, solver)));
    solver.post(IntConstraintFactory.sum(new IntVar[] {t,w,o}, VariableFactory.fixed(2, solver)));
    solver.post(IntConstraintFactory.sum(new IntVar[] {t,h,r,e,e}, VariableFactory.fixed(3, solver)));
    solver.post(IntConstraintFactory.sum(new IntVar[] {f,o,u,r}, VariableFactory.fixed(4, solver)));
    solver.post(IntConstraintFactory.sum(new IntVar[] {f,i,v,e}, VariableFactory.fixed(5, solver)));
    solver.post(IntConstraintFactory.sum(new IntVar[] {s,i,x}, VariableFactory.fixed(6, solver)));
    solver.post(IntConstraintFactory.sum(new IntVar[] {s,e,v,e,n}, VariableFactory.fixed(7, solver)));
    solver.post(IntConstraintFactory.sum(new IntVar[] {e,i,g,h,t}, VariableFactory.fixed(8, solver)));
    solver.post(IntConstraintFactory.sum(new IntVar[] {n,i,n,e}, VariableFactory.fixed(9, solver)));
    solver.post(IntConstraintFactory.sum(new IntVar[] {t,e,n}, VariableFactory.fixed(10, solver)));
    solver.post(IntConstraintFactory.sum(new IntVar[] {e,l,e,v,e,n}, VariableFactory.fixed(11, solver)));
    solver.post(IntConstraintFactory.sum(new IntVar[] {t,w,e,l,f}, VariableFactory.fixed(12, solver)));

  }


  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(A));
  }
  
  @Override
  public void solve() {
    solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, AMax);
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_sol = 0;
      do {
        System.out.println("\nAMax: " + AMax.getValue());
        for (int I = 0; I < N; I++) {
          System.out.print(A[I].getValue() + " ");
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

    new CalculsDEnfer().execute(args);

  }


} // end class
 
