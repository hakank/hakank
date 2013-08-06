/*

  Hidato puzzle in Gecode.

  http://www.shockwave.com/gamelanding/hidato.jsp
  http://www.hidato.com/
 
  """
  Puzzles start semi-filled with numbered tiles.
  The first and last numbers are circled.
  Connect the numbers together to win. Consecutive
  number must touch horizontally, vertically, or
  diagonally.
  """


  Compare with the following models: 
  * MiniZinc: http://www.hakank.org/minizinc/hidato.mzn
  * Comet   : http://www.hakank.org/comet/hidato.co


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/



#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

namespace {

  // List of problems
  extern const int* problems[];

  // Number of specifications
  extern const unsigned int n_examples;

}



class Hidato : public Script {
protected:

  // Simple problem
  const int* prob;

  IntVarArray board;       // the board

  // Return rows (and columns) of matrix
  int rows(void) const {
    return prob[0];
  }

public:

  Hidato(const SizeOptions& opt) 
    : 
    prob(problems[opt.size()]),
    board(*this, rows()*rows(), 1, rows()*rows())
  {

    int n = rows();

    Matrix<IntVarArray> m(board, n, n);
    int p = 1; // position in problem
    // place the fixed tiles
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        if (prob[p] > 0) {
          rel(*this, m(j, i) == prob[p], opt.icl());
        }
        p++;
      }
    }
    

    //
    // place all integers from 1..r*c
    //
    distinct(*this, board, opt.icl());

    /**
     * In the MiniZinc and Comet models this was implemented as four
     * exists/tryall loops for i,j, a, and b. 
     * Since Gecode don't support that constructs we use another 
     * approach, but the same principle.
     */
    for(int k = 1; k < n*n; k++) {
      IntVar i(*this, 0, n-1);
      IntVar j(*this, 0, n-1);
      IntVar a(*this, -1, 1);
      IntVar b(*this, -1, 1);

      // 1) First: fix this k, i.e.
      //        k == board[i*n+j]
      IntVar inj(*this, 0,n*n-1);
      rel(*this, inj == i*n+j, opt.icl());
      element(*this, board, inj, k, opt.icl());

      // 2) Then, find the position of the next value, i.e.
      //     k + 1 == board[(i+a)*n+(j+b)]
      IntVar ai(*this, 0, n-1); // this takes care of the borders of the matrix
      IntVar jb(*this, 0, n-1); // ibid.
      rel(*this, a+i == ai,opt.icl());
      rel(*this, j+b == jb,opt.icl());
      IntVar ai_plus_jb(*this, 0,n*n-1);
      rel(*this, ai_plus_jb == ai*n+jb, opt.icl());
      // post(*this, ai_plus_jb == (a+i)*n+j+b, opt.icl()); // shorter, but less efficient (and we must use the border checks)
      element(*this, board, ai_plus_jb, k+1, opt.icl());

    }

    // branching (inspired by the Comet and Gecode Nonogram models)
    for (int i = 0; i < n; ++i) {
      branch(*this, m.row(i), INT_VAR_NONE(), INT_VAL_SPLIT_MAX());
      branch(*this, m.col(i), INT_VAR_NONE(), INT_VAL_SPLIT_MAX());
    }

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    int n = rows();
    Matrix<IntVarArray> m(board, n, n);
    os << "Result: " << std::endl;
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        os.width(3);
        os << m(j,i) << " ";
      }
      os << std::endl;
    }
    os << std::endl;

  }


  // Constructor for cloning s
  Hidato(bool share, Hidato& s) : Script(share,s), prob(s.prob) {
    board.update(*this, share, s.board);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Hidato(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  SizeOptions opt("Hidato");

  opt.solutions(0);
  opt.icl(ICL_DOM);
  opt.parse(argc,argv);
  if (!opt.size()) {
    opt.size(0);
  }
  if (opt.size() >= n_examples) {
    std::cout << "Examples are between 0 and " << n_examples-1 << "." << std::endl;
    return 1;
  }

  Script::run<Hidato,DFS,SizeOptions>(opt);

  return 0;
}



namespace {

  /** Problem specifications
   *
   * A specification is given by a list of integers. The first
   * integer (n) specify the number of rows/columns. 
   * Then n groups of integers follows with the matrix values,
   * a 0 for unknown value.
   *
   */

  //
  // Problem 1 .. 3 is from the ECLiPSe program cited above.
  //

  // Specification for problem 0
  const int p0[] =
    { 3,
      6,0,9,
      0,2,8,
      1,0,0
    };

 
  // Specification for problem 1
  const int p1[] =
    { 7,
      0,44,41, 0, 0, 0, 0,
      0,43, 0,28,29, 0, 0,
      0, 1, 0, 0, 0,33, 0,
      0, 2,25, 4,34, 0,36,
      49,16, 0,23, 0, 0,0,
      0,19, 0, 0,12, 7, 0,
      0, 0, 0,14, 0, 0, 0 
    };

    
  // Problems from the book:
  // Gyora Bededek: "Hidato: 2000 Pure Logic Puzzles"
      
  // Specification for problem 2
  // "Problem 1 (Practice)"
  const int p2[] =
    { 5,
      0, 0,20, 0, 0,
      0, 0, 0,16,18,
      22, 0,15, 0, 0,
      23, 0, 1,14,11,
      0,25, 0, 0,12
    };

  // Specification for problem 3
  // "problem 2 (Practice)"
  const int p3[] =
    { 5,
      0, 0, 0, 0,14,
      0,18,12, 0, 0,
      0, 0,17, 4, 5,
      0, 0, 7, 0, 0,
      9, 8,25, 1, 0
    };

  // Specification for problem 4
  // "problem 3 (Beginner)"
  const int p4[] = 
    { 6,
      0, 26,0,0,0,18,
      0,0,27,0,0,19,
      31,23,0,0,14,0,
      0,33,8,0,15,1,
      0,0,0,5,0,0,
      35,36,0,10,0,0
    };

  // Specification for problem 5
  // "Problem 15 (Intermediate)"
  const int p5[] = 
    { 8,
      64, 0, 0, 0, 0, 0, 0, 0,
      1,63, 0,59,15,57,53, 0,
      0, 4, 0,14, 0, 0, 0, 0,
      3, 0,11, 0,20,19, 0,50,
      0, 0, 0, 0,22, 0,48,40,
      9, 0, 0,32,23, 0, 0,41,
      27, 0, 0, 0,36, 0,46, 0,
      28,30, 0,35, 0, 0, 0, 0
    };

  //  Problem 15 (Intermediate)
  const int p6[] = 
    { 8,
      64, 0, 0, 0, 0, 0, 0, 0,
      1,63, 0,59,15,57,53, 0,
      0, 4, 0,14, 0, 0, 0, 0,
      3, 0,11, 0,20,19, 0,50,
      0, 0, 0, 0,22, 0,48,40,
      9, 0, 0,32,23, 0, 0,41,
      27, 0, 0, 0,36, 0,46, 0,
      28,30, 0,35, 0, 0, 0, 0
    };
  
  
  // Problem 156 (Master)
  // (This is harder to solve than the 12x12 prolem 188 below...%)
  const int p7[] = 
    { 10,
      88, 0, 0,100, 0, 0,37,0, 0,34,
      0,86, 0,96,41, 0, 0,36, 0, 0,
      0,93,95,83, 0, 0, 0,31,47, 0,
      0,91, 0, 0, 0, 0, 0,29, 0, 0,
      11, 0, 0, 0, 0, 0, 0,45,51, 0,
      0, 9, 5, 3, 1, 0, 0, 0, 0, 0,
      0,13, 4, 0, 0, 0, 0, 0, 0, 0,
      15, 0, 0,25, 0, 0,54,67, 0, 0,
      0,17, 0,23, 0,60,59, 0,69, 0,
      19, 0,21,62,63, 0, 0, 0, 0, 0
    };


  // Problem 188 Genius)
  const int p8[] = 
    { 12,
      0,  0,134,  2,  4,  0,  0,  0,  0,  0,  0,  0,
      136,  0,  0,  1,  0,  5,  6, 10,115,106,  0,  0,
      139,  0,  0,124,  0,122,117,  0,  0,107,  0,  0,
      0,131,126,  0,123,  0,  0, 12,  0,  0,  0,103,
      0,  0,144,  0,  0,  0,  0,  0, 14,  0, 99,101,
      0,  0,129,  0, 23, 21,  0, 16, 65, 97, 96,  0,
      30, 29, 25,  0,  0, 19,  0,  0,  0, 66, 94,  0,
      32,  0,  0, 27, 57, 59, 60,  0,  0,  0,  0, 92,
      0, 40, 42,  0, 56, 58,  0,  0, 72,  0,  0,  0,
      0, 39,  0,  0,  0,  0, 78, 73, 71, 85, 69,  0,
      35,  0,  0, 46, 53,  0,  0,  0, 80, 84,  0,  0,
      36,  0, 45,  0,  0, 52, 51,  0,  0,  0,  0, 88
    };



  const int *problems[] = {p0, p1, p2, p3, p4, p5, p6, p7, p8};
  const unsigned n_examples = sizeof(problems)/sizeof(int*);
  
}


