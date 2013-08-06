/**
 *
 * KenKen puzzle in Choco3.
 *
 *
 * http://en.wikipedia.org/wiki/KenKen
 * """
 * KenKen or KEN-KEN is a style of arithmetic and logical puzzle sharing
 * several characteristics with sudoku. The name comes from Japanese and
 * is translated as 'square wisdom' or 'cleverness squared'.
 * ...
 * The objective is to fill the grid in with the digits 1 through 6 such that:
 *
 * * Each row contains exactly one of each digit
 * * Each column contains exactly one of each digit
 * * Each bold-outlined group of cells is a cage containing digits which
 *   achieve the specified result using the specified mathematical operation:
 *     addition (+),
 *     subtraction (-),
 *     multiplication (x),
 *     and division (/).
 *    (Unlike in Killer sudoku, digits may repeat within a group.)
 *
 * ...
 * More complex KenKen problems are formed using the principles described
 * above but omitting the symbols +, -, x and /, thus leaving them as
 * yet another unknown to be determined.
 * """
 *
 * The solution is:
 *
 *    5 6 3 4 1 2
 *    6 1 4 5 2 3
 *    4 5 2 3 6 1
 *    3 4 1 2 5 6
 *    2 3 6 1 4 5
 *    1 2 5 6 3 4
 *
 *
 * Choco3 model by Hakan Kjellerstrand (hakank@gmail.com)
 * http://www.hakank.org/choco3/
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
import solver.search.strategy.strategy.AbstractStrategy;
import solver.search.strategy.selectors.variables.*;
import util.ESat;
import util.tools.ArrayUtils;

public class KenKen2 extends AbstractProblem {


  // size of matrix
  int n = 6;

  // For a better view of the problem, see
  //  http://en.wikipedia.org/wiki/File:KenKenProblem.svg
  
  // hints
  //  sum, the hints
  // Note: this is 1-based (fixed below)
  int[][] problem =
  {
    new int[] { 11,  1,1, 2,1},
    new int[] {  2,  1,2, 1,3},
    new int[] { 20,  1,4, 2,4},
    new int[] {  6,  1,5, 1,6, 2,6, 3,6},
    new int[] {  3,  2,2, 2,3},
    new int[] {  3,  2,5, 3,5},
    new int[] {240,  3,1, 3,2, 4,1, 4,2},
    new int[] {  6,  3,3, 3,4},
    new int[] {  6,  4,3, 5,3},
    new int[] {  7,  4,4, 5,4, 5,5},
    new int[] { 30,  4,5, 4,6},
    new int[] {  6,  5,1, 5,2},
    new int[] {  9,  5,6, 6,6},
    new int[] {  8,  6,1, 6,2, 6,3},
    new int[] {  2,  6,4, 6,5}
  };
  
  int num_p = problem.length; // Number of segments
  
 
  IntVar[][] x;

  //
  // Decomposition of product(IntVar[] v) 
  //   returns IntVar: the product of elements in array v
  //
  // Assumption: all element are >= 0.
  //
  public IntVar product(IntVar[] v) {
    int len = v.length;
    int max_val = 1; // maximum possible value
    int min_val = 1;
    for(int i = 0; i < len; i++) {
      max_val *= v[i].getUB();
      min_val *= v[i].getLB();
    }
    IntVar[] prod = VariableFactory.boundedArray("prod", len, min_val, max_val, solver);
    prod[0] = v[0];
    for(int i = 1; i < len; i++) {
      solver.post(IntConstraintFactory.times(prod[i-1], v[i], prod[i]));
    }

    return prod[len-1];

  }


  /**
   * Ensure that the sum of the segments
   * in cc == res
   *
   */
  public void calc(int[] cc,
                   int res)
  {
    IntVar resVar = VariableFactory.fixed(res, solver);

    int ccLen = cc.length;

    if (ccLen == 4) {

      // for two operands there's a lot of possible variants
      IntVar a = x[cc[0]-1][cc[1]-1];
      IntVar b = x[cc[2]-1][cc[3]-1];

      // a+b=res
      BoolVar r1 = VariableFactory.bool("r1", solver);
      solver.post(IntConstraintFactory.implies(r1,
                                               IntConstraintFactory.arithm(a,"+",b,"=",res)));
      solver.post(IntConstraintFactory.implies(VariableFactory.not(r1),
                                               IntConstraintFactory.arithm(a,"+",b,"!=",res)));

      // a*b=res
      IntVar t2 = VariableFactory.bounded("t2", 0, a.getUB()*b.getUB(), solver);
      solver.post(IntConstraintFactory.times(a,b,t2));
      BoolVar r2 = VariableFactory.bool("r2", solver);
      solver.post(IntConstraintFactory.implies(r2,
                                               IntConstraintFactory.arithm(t2,"=",resVar)));
      solver.post(IntConstraintFactory.implies(VariableFactory.not(r2),
                                               IntConstraintFactory.arithm(t2,"!=",resVar)));
      
      // a*res=b (b/a=res)
      IntVar t3 = VariableFactory.bounded("t3", 0, a.getUB()*res, solver);
      solver.post(IntConstraintFactory.times(a,resVar, t3));
      BoolVar r3 = VariableFactory.bool("r3", solver);
      solver.post(IntConstraintFactory.implies(r3,
                                               IntConstraintFactory.arithm(t3,"=",b)));
      solver.post(IntConstraintFactory.implies(VariableFactory.not(r3),
                                               IntConstraintFactory.arithm(t3,"!=",b)));

      // b*res=a (a/b=res)
      IntVar t4 = VariableFactory.bounded("t4", 0, b.getUB()*res, solver);
      solver.post(IntConstraintFactory.times(b,resVar, t4));
      BoolVar r4 = VariableFactory.bool("r4", solver);
      solver.post(IntConstraintFactory.implies(r4,
                                               IntConstraintFactory.arithm(t4,"=",a)));
      solver.post(IntConstraintFactory.implies(VariableFactory.not(r4),
                                               IntConstraintFactory.arithm(t4,"!=",a)));

      // a-b=res
      BoolVar r5 = VariableFactory.bool("r5", solver);
      solver.post(IntConstraintFactory.implies(r5,
                                               IntConstraintFactory.arithm(a,"-",b,"=",res)));
      solver.post(IntConstraintFactory.implies(VariableFactory.not(r5),
                                               IntConstraintFactory.arithm(a,"-",b,"!=",res)));
      
      // b-a=res
      BoolVar r6 = VariableFactory.bool("r6", solver);
      solver.post(IntConstraintFactory.implies(r6,
                                               IntConstraintFactory.arithm(b,"-",a,"=",res)));
      solver.post(IntConstraintFactory.implies(VariableFactory.not(r6),
                                               IntConstraintFactory.arithm(b,"-",a,"!=",res)));


      // r1+r2+r3+r4+r5+r6 >= 1
      IntVar s = VariableFactory.enumerated("s", 0, 6,solver);
      solver.post(IntConstraintFactory.sum(new BoolVar[] {r1,r2,r3,r4,r5,r6}, s));
      solver.post(IntConstraintFactory.arithm(s, ">=", 1));


    } else {

      // For length > 2 then res is either the sum
      // the the product of the segment

      // sum the numbers
      int len = cc.length / 2;
      IntVar[] xx = VariableFactory.enumeratedArray("xx", len, 0, n, solver);
      for(int i = 0; i < len; i++) {
        xx[i] = x[cc[i*2]-1][cc[i*2+1]-1];
      }


      // Sum
      IntVar ssum = VariableFactory.bounded("ssum", 0, n*len, solver);
      solver.post(IntConstraintFactory.sum(xx,ssum));
      BoolVar rsum = VariableFactory.bool("rsum", solver);
      solver.post(IntConstraintFactory.implies(rsum,
                                               IntConstraintFactory.arithm(ssum,"=",resVar)));
      solver.post(IntConstraintFactory.implies(VariableFactory.not(rsum),
                                               IntConstraintFactory.arithm(ssum,"!=",resVar)));


      // Product
      IntVar prod = product(xx);
      BoolVar rprod = VariableFactory.bool("rprod", solver);
      solver.post(IntConstraintFactory.implies(rprod,
                                               IntConstraintFactory.arithm(prod,"=",resVar)));
      solver.post(IntConstraintFactory.implies(VariableFactory.not(rprod),
                                               IntConstraintFactory.arithm(prod,"!=",resVar)));

      solver.post(IntConstraintFactory.arithm(rsum, "+", rprod, ">=", 1));

    }

  }




  @Override
  public void buildModel() {
    
    x = VariableFactory.enumeratedMatrix("x", n, n, 1, n, solver);

    // Alldifferent rows and columns
    for(int i = 0; i < n; i++) {
      solver.post(IntConstraintFactory.alldifferent(x[i], "BC"));
      solver.post(IntConstraintFactory.alldifferent(ArrayUtils.getColumn(x, i), "BC"));
    }

    // Calculate the segments
    for(int i = 0; i < num_p; i++) {
      int[] segment = problem[i];

      // Remove the sum from the segment
      int len = segment.length;
      int[] s2 = new int[len-1];
      System.arraycopy(segment, 1, s2, 0, len-1);

      calc(s2, segment[0]);

    }

  

  }

  @Override
  public void createSolver() {
    solver = new Solver("KenKen2");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.inputOrder_InDomainMin(ArrayUtils.flatten(x)));
  }

  @Override
  public void solve() {
    solver.findSolution();
  }


  @Override
  public void prettyOut() {

    if (solver.isFeasible() == ESat.TRUE) {
      int num_solutions = 0;
      do {

        for(int i = 0; i < n; i++) {
          for(int j = 0; j < n; j++) {
              System.out.print(x[i][j].getValue() + " ");
          }
          System.out.println();
        }
        System.out.println();

        num_solutions++;

      } while (solver.nextSolution() == Boolean.TRUE);
      
      System.out.println("It was " + num_solutions + " solutions.");
      
    }  else {
      System.out.println("No solution.");
    }
    
  }


  public static void main(String args[]) {

    new KenKen2().execute(args);

  }

}

