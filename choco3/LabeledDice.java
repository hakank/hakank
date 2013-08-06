/**
 *
 * Labeled dice problem in Choco3.
 *
 * Labeled dice problem.
 *
 * From Jim Orlin 'Colored letters, labeled dice: a logic puzzle'
 * http://jimorlin.wordpress.com/2009/02/17/colored-letters-labeled-dice-a-logic-puzzle/
 * """
 * My daughter Jenn bough a puzzle book, and showed me a cute puzzle.  There
 * are 13 words as follows:  BUOY, CAVE, CELT, FLUB, FORK, HEMP, JUDY,
 * JUNK, LIMN, QUIP, SWAG, VISA, WISH.
 *
 * There are 24 different letters that appear in the 13 words.  The question
 * is:  can one assign the 24 letters to 4 different cubes so that the
 * four letters of each word appears on different cubes.  (There is one
 * letter from each word on each cube.)  It might be fun for you to try
 * it.  I'll give a small hint at the end of this post. The puzzle was
 * created by Humphrey Dudley.
 * """
 *
 * Jim Orlin's followup 'Update on Logic Puzzle':
 * http://jimorlin.wordpress.com/2009/02/21/update-on-logic-puzzle/
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

public class LabeledDice extends AbstractProblem {

  @Option(name = "-symmetry_breaking", usage = "Use symmetry breaking (default false).", required = false)
  boolean symmetry_breaking = false;


  //
  // Data
  //
  int n = 4;
  int m = 24;
  
  int A = 0;
  int B = 1;
  int C = 2;
  int D = 3;
  int E = 4;
  int F = 5;
  int G = 6;
  int H = 7;
  int I = 8;
  int J = 9;
  int K = 10;
  int L = 11;
  int M = 12;
  int N = 13;
  int O = 14;
  int P = 15;
  int Q = 16;
  int R = 17;
  int S = 18;
  int T = 19;
  int U = 20;
  int V = 21;
  int W = 22;
  int Y = 23;
  

  String[] letters_str = {"A","B","C","D","E","F","G","H","I","J","K","L","M",
                          "N","O","P","Q","R","S","T","U","V","W","Y"};
  
  int num_words = 13;
  int[][] words =
  {
        {B,U,O,Y},
        {C,A,V,E},
        {C,E,L,T},
        {F,L,U,B},
        {F,O,R,K},
        {H,E,M,P},
        {J,U,D,Y},
        {J,U,N,K},
        {L,I,M,N},
        {Q,U,I,P},
        {S,W,A,G},
        {V,I,S,A},
        {W,I,S,H}
        };

  IntVar[] dice;
  IntVar[] gcc;


  @Override
  public void buildModel() {

    dice = VariableFactory.enumeratedArray("dice", m, 0, n-1, solver);
    gcc = VariableFactory.enumeratedArray("gcc", n, 6, 6, solver);

    // the letters in a word must be on a different die
    for(int i = 0; i < num_words; i++) {
      IntVar[] t = VariableFactory.enumeratedArray("t", n, 0, m, solver);
      for(int j = 0; j < n; j++) {
        t[j] = dice[words[i][j]];
      }
      solver.post(IntConstraintFactory.alldifferent(t,"BC"));
    }
    
    // there must be exactly 6 letters of each die
    int[] d = ArrayUtils.zeroToN(n);
    boolean closed = true;
    solver.post(IntConstraintFactory.global_cardinality(dice, d, gcc, closed));

    // Symmetry breaking: Place first letter on cube 1
    // This yield only 6 solutions (instead of 24).
    if (symmetry_breaking) {
      solver.post(IntConstraintFactory.arithm(dice[0], "=", VariableFactory.fixed(0, solver)));
    }

  }

  @Override
  public void createSolver() {
    solver = new Solver("LabeledDice");
  }

  @Override
  public void configureSearch() {
    solver.set(IntStrategyFactory.firstFail_InDomainMin(ArrayUtils.append(dice, gcc)));
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
        for(int d = 0; d < n; d++) {
          System.out.print("die " + d + ": ");
          for(int i = 0; i < m; i++) {
            if (dice[i].getValue() == d) {
              System.out.print(letters_str[i] + " ");
            }
          }
          System.out.println();
        }

        num_solutions++;

      } while (solver.nextSolution() == Boolean.TRUE);
      
      System.out.println("It was " + num_solutions + " solutions.");
      
    }  else {
      System.out.println("No solution.");
    }
    
  }


  public static void main(String args[]) {

    new LabeledDice().execute(args);

  }


}

