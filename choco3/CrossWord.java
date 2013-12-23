/**
 *
 * Cross word problem in Choco3
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
import solver.search.loop.monitors.SearchMonitorFactory;
import solver.variables.IntVar;
import solver.variables.BoolVar;
import solver.variables.VariableFactory;
import solver.search.strategy.strategy.AbstractStrategy;
import util.ESat;
import util.tools.ArrayUtils;

public class CrossWord extends AbstractProblem {

   
  String letters = "abcdefghijklmnopqrstuvwxyz";
  String[] letters_array;
  String[] alpha = {"_","a","b","c","d","e","f",
                    "g","h","i","j","k","l","m",
                    "n","o","p","q","r","s","t",
                    "u","v","w","x","y","z"};
  


  IntVar[] all;

  IntVar E[];
  IntVar[][] A;
  int N = 8;
  int word_len = 5;
  int num_words = 15;
  int a=1;  int b=2;  int c=3; int d=4;  int e=5;  int f=6;
  int g=7;  int h=8;  int i=9; int j=10; int k=11; int l=12;
  int m=13; int n=14; int o=15; int p=16; int q=17; int r=18;
  int s=19; int t=20; int u=21; int v=22; int w=23; int x=24;
  int y=25; int z=26;
  
  
  
  
  int[][] AA = {{h, o, s, e, s},  //  HOSES
                {l, a, s, e, r},  //  LASER
                {s, a, i, l, s},  //  SAILS
                {s, h, e, e, t},  //  SHEET
                {s, t, e, e, r},  //  STEER
                {h, e, e, l, 0},  //  HEEL
                {h, i, k, e, 0},  //  HIKE
                {k, e, e, l, 0},  //  KEEL
                {k, n, o, t, 0},  //  KNOT
                {l, i, n, e, 0},  //  LINE
                {a, f, t, 0, 0},  //  AFT
                {a, l, e, 0, 0},  //  ALE
                {e, e, l, 0, 0},  //  EEL
                {l, e, e, 0, 0},  //  LEE
                {t, i, e, 0, 0}}; //  TIE
  
  int num_overlapping = 12;
  int[][] overlapping = {{0, 2, 1, 0},  //  s
                         {0, 4, 2, 0},  //  s
                         
                         {3, 1, 1, 2},  //  i
                         {3, 2, 4, 0},  //  k
                         {3, 3, 2, 2},  //  e
                         
                         {6, 0, 1, 3},  //  l
                         {6, 1, 4, 1},  //  e
                         {6, 2, 2, 3},  //  e
                         
                         {7, 0, 5, 1},  //  l
                         {7, 2, 1, 4},  //  s
                         {7, 3, 4, 2},  //  e
                         {7, 4, 2, 4}}; //  r
  

  @Override
  public void buildModel() {

    /**
     *
     *     1   2   3   4   5
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
     * 6 |   | # | # |   | # |  The numbers 1,2,3,4,5,6,7,8 in the crossword
     *   +---+---+---+---+---+  puzzle correspond to the words 
     *
     */

    //
    // variables
    //
    A = new IntVar[num_words][word_len];
    // for labeling on A and E
    all = new IntVar[(num_words * word_len) + N];
    IntVar[] A_flat = new IntVar[num_words * word_len];

    for(int I = 0; I < num_words; I++) {
      for(int J = 0; J < word_len; J++) {
        A[I][J] = VariableFactory.enumerated("A[" + I + "," + J + "]", 0, 26, solver);
        A_flat[I * word_len + J] = A[I][J];
        all[I * word_len + J] = A[I][J];
      }
    }

    E = VariableFactory.boundedArray("E", N, 0, num_words, solver);
    for(int I = 0; I < N; I++) {
      all[num_words * word_len + I] = E[I];
    }


    //
    // constraints
    //
    solver.post(IntConstraintFactory.alldifferent(E, "BC"));

    for(int I = 0; I < num_words; I++) {
      for(int J = 0; J < word_len; J++) {
        solver.post(IntConstraintFactory.arithm(A[I][J], "=", VariableFactory.fixed(AA[I][J], solver)));
      }
    }

    for(int I = 0; I < num_overlapping; I++) {
      /*
        // MiniZinc
        forall(i in 1..num_overlapping) (
             A[E[overlapping[i,1]], overlapping[i,2]] =  A[E[overlapping[i,3]], overlapping[i,4]]
         )
      */

      IntVar vv = VariableFactory.bounded("v_"+I, 0, num_words*word_len, solver);
      
      IntVar w1 = VariableFactory.bounded("w1_"+I, 0, num_words*word_len, solver);
      solver.post(IntConstraintFactory.times(E[overlapping[I][0]], VariableFactory.fixed(word_len,solver), w1));
      IntVar s1 = VariableFactory.bounded("s1_"+I, 0, num_words*word_len*2, solver);
      solver.post(IntConstraintFactory.sum(new IntVar[]{w1,VariableFactory.fixed(overlapping[I][1], solver)}, s1));
      solver.post(IntConstraintFactory.element(vv, A_flat, s1, 0));


      IntVar w2 = VariableFactory.bounded("w2_"+I, 0, num_words*word_len, solver);
      solver.post(IntConstraintFactory.times(E[overlapping[I][2]], VariableFactory.fixed(word_len,solver), w2));
      IntVar s2 = VariableFactory.bounded("s2_"+I, 0, num_words*word_len*2, solver);
      solver.post(IntConstraintFactory.sum(new IntVar[]{w2,VariableFactory.fixed(overlapping[I][3], solver)}, s2));
      solver.post(IntConstraintFactory.element(vv, A_flat, s2, 0));

    }

  }

  @Override
  public void createSolver() {
    solver = new Solver("CrossWord");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.inputOrder_InDomainMin(all));
  }

  @Override
  public void solve() {
    solver.findSolution();
  }

  @Override
  public void prettyOut() {

    if(solver.isFeasible() == ESat.TRUE) {
      do {

        System.out.println("E:");
        for(int ee = 0; ee < N; ee++) {
          int e_val = (int)E[ee].getValue();
          System.out.format("%d: (%2d) ", ee, e_val);
          for(int ii = 0; ii < word_len; ii++) {
          System.out.print(alpha[(int)A[ee][ii].getValue()]);
          }
        System.out.println();
        }
        
        System.out.println();


      } while (solver.nextSolution() == Boolean.TRUE);

    } else {

      System.out.println("Problem is not feasible.");

    }


  }


  //
  // main
  //
  public static void main(String args[]) {

    new CrossWord().execute(args);

  }
}

