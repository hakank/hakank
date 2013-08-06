/*

  Killer Sudoko in Gecode.

  http://en.wikipedia.org/wiki/Killer_Sudoku
  """
  Killer sudoku (also killer su doku, sumdoku, sum doku, addoku, or 
  samunamupure) is a puzzle that combines elements of sudoku and kakuro. 
  Despite the name, the simpler killer sudokus can be easier to solve 
  than regular sudokus, depending on the solver's skill at mental arithmetic; 
  the hardest ones, however, can take hours to crack.

  ...

  The objective is to fill the grid with numbers from 1 to 9 in a way that 
  the following conditions are met:

    * Each row, column, and nonet contains each number exactly once.
    * The sum of all numbers in a cage must match the small number printed 
      in its corner.
    * No number appears more than once in a cage. (This is the standard rule 
      for killer sudokus, and implies that no cage can include more 
      than 9 cells.)

  In 'Killer X', an additional rule is that each of the long diagonals 
  contains each number once.
  """

  Here we solve the problem from the Wikipedia page, also shown here
  http://en.wikipedia.org/wiki/File:Killersudoku_color.svg

  And the solution:
  http://commons.wikimedia.org/wiki/File:Killersudoku_color_solution.svg


  Compare with the following models:
  * Comet   : http://www.hakank.org/comet/killer_sudoku.co
  * MiniZinc: http://www.hakank.org/minizinc/killer_sudoku.mzn
  * SICStus: http://www.hakank.org/sicstus/killer_sudoku.pl
  * ECLiPSe: http://www.hakank.org/eclipse/killer_sudoku.ecl

  This Gecode model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Gecode page: http://www.hakank.org/gecode/

 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include "gecode/minimodel.hh"

using namespace Gecode;

using std::cout;
using std::endl;

class KillerSudoku : public Script {
protected:

  static const int n = 9;

  IntVarArray x;

public:

  // Actual model
  KillerSudoku(const SizeOptions& opt) : 
    x(*this, n*n, 1, n)
  {

    int num_p = 29; // number of segments
    int num_hints = 4; // number of hints per segment

    int _P[] = {
      1,1,  1,2, 0,0, 0,0,   3,
      1,3,  1,4, 1,5, 0,0,  15,
      1,6,  2,5, 2,6, 3,5,  22,
      1,7,  2,7, 0,0, 0,0,   4,
      1,8,  2,8, 0,0, 0,0,  16,
      1,9,  2,9, 3,9, 4,9,  15,
      2,1,  2,2, 3,1, 3,2,  25,
      2,3,  2,4, 0,0, 0,0,  17,
      3,3,  3,4, 4,4, 0,0,   9,
      3,6,  4,6, 5,6, 0,0,   8,
      3,7,  3,8, 4,7, 0,0,  20,
      4,1,  5,1, 0,0, 0,0,   6,
      4,2,  4,3, 0,0, 0,0,  14,
      4,5,  5,5, 6,5, 0,0,  17,
      4,8,  5,7, 5,8, 0,0,  17,
      5,2,  5,3, 6,2, 0,0,  13,
      5,4,  6,4, 7,4, 0,0,  20,
      5,9,  6,9, 0,0, 0,0,  12,
      6,1,  7,1, 8,1, 9,1,  27,
      6,3,  7,2, 7,3, 0,0,   6,
      6,6,  7,6, 7,7, 0,0,  20,
      6,7,  6,8, 0,0, 0,0,   6,
      7,5,  8,4, 8,5, 9,4,  10,
      7,8,  7,9, 8,8, 8,9,  14,
      8,2,  9,2, 0,0, 0,0,   8,
      8,3,  9,3, 0,0, 0,0,  16,
      8,6,  8,7, 0,0, 0,0,  15,
      9,5,  9,6, 9,7, 0,0,  13,
      9,8,  9,9, 0,0, 0,0,  17
    };

    IntArgs P(num_p*(2*num_hints+1), _P);


    // The usual Sudoku constraints
    // rows and columns
    Matrix<IntVarArray> m(x, n, n);
    for(int i = 0; i < n; i++) {
      distinct(*this, m.row(i), opt.icl());
      distinct(*this, m.col(i), opt.icl());
    }
    // and the squares
    int nn = 3;
    for (int i=0; i < n; i+=n) {
      for (int j=0; j < n; j+=n) {
        distinct(*this, m.slice(i, i+nn, j, j+nn), opt.icl());
      }
    }

    // calculate the hints
    for(int p = 0; p < num_p; p++) {
      IntVarArgs p_tmp;
      for(int i = 0; i < num_hints; i++) {
        int p_test = P[p*9 + 2*i+0];
        if (p_test > 0) {
          int p1 = P[p*9+2*i+0]-1;
          int p2 = P[p*9+2*i+1]-1;
          p_tmp << expr(*this, x[p1*n+p2]);
        }
      }
      int p_sum = P[p*9+2*num_hints];
      rel(*this, sum(p_tmp) == p_sum, opt.icl());
    }

    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Constructor for cloning s
  KillerSudoku(bool share, KillerSudoku& s) : Script(share,s) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new KillerSudoku(share,*this);
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "Solution: " << endl;
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        os << x[i*n+j] << " ";        
      }
      os << endl;
    }
    os << endl;
  }

};

/** 
 *  main
 */
int
main(int argc, char* argv[]) {
  SizeOptions opt("KillerSudoku");
  opt.solutions(0);
  opt.icl(ICL_DOM);

  opt.parse(argc,argv);

  Script::run<KillerSudoku,DFS,SizeOptions>(opt);

  return 0;
}

