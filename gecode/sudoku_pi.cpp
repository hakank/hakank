/*

  Pi Day Sudoku (2009) problem  in Gecode.

  Pi Day Sudoku 2009

  Via 360 "Pi Day Sudoku 2009"
  http://threesixty360.wordpress.com/2009/03/09/pi-day-sudoku-2009/
  """
  Each row, column, and region contains the digits 1-9 exactly once plus 
  three Pi symbols.  There' a printable .pdf file here 
  [http://brainfreezepuzzles.com/main/files/Brainfreeze_PiDay2009.pdf]

  As a bonus, if you send a correct solution in to Brainfreeze puzzles in 
  the next couple months, you're eligible for a drawing for their book 
  on Color Sudoku!  More details are on their website [[http://brainfreezepuzzles.com/]].
  """

  Also, see http://brainfreezepuzzles.com/main/piday2009.html


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/sudoku_pi.mzn
  * Comet: http://www.hakank.org/minizinc/sudoku_pi.co

  I wrote about the MiniZinc and Comet models in 
  "Pi Day Sudoku 2009 - the models (MiniZinc and Comet)"
  http://www.hakank.org/constraint_programming_blog/2010/03/pi_day_sudoku_2009_the_models_1.html


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

  static const int n = 12;
  static const int X = 0;
  static const int P = -1; // Pi

  IntVarArray x;

public:

  // Actual model
  KillerSudoku(const SizeOptions& opt) : 
    x(*this, n*n, -1, n)
  {

    // the occurrences of the digits -1..9
    // to be used with the global cardinality constraint
    int _occ[] = {3, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1};
    IntArgs occ(11, _occ);

    int _dat[] = 
      {
        4,9,7, P,5,X,X,X,X, X,X,X,
        X,P,X, 8,X,X,9,6,1, 5,2,X,
        X,8,X, 1,X,X,X,P,X, 7,X,X,
        X,X,X, X,X,X,X,P,X, 4,X,X,
        5,3,9, 6,X,X,X,X,X, X,X,X,
        
        9,4,X, P,P,P,7,X,X, X,X,X,
        X,X,X, X,X,6,2,5,P, X,7,4,
        X,X,X, X,X,X,X,X,P, P,3,8,
        X,7,8, 4,6,9,X,X,X, X,X,X,
        
        X,X,3, X,P,X,X,4,7, 1,6,9,
        X,X,4, X,1,X,X,X,6, X,P,X,
        X,X,X, X,X,X,X,X,4, X,5,X
      };
    IntArgs dat(n*n, _dat);

    //
    // The regions
    // 
    int num_regions = 12;
    int _regions[] = {

      // Upper left dark green
      1,1  , 1,2  , 1,3  , 
      2,1  , 2,2  , 2,3  , 
      3,1  , 3,2  , 
      4,1  , 4,2  ,  
      5,1  , 5,2  , 
      
      // Upper mid light dark green
      1,4  ,  1,5  ,  1,6  ,  1,7  ,  1,8  ,  1,9  , 
      2,4  ,  2,5  ,  2,6  ,  2,7  ,  2,8  ,  2,9  , 
      
      // Upper right green
      1,10  ,  1,11  ,  1,12  , 
      2,10  ,  2,11  ,  2,12  , 
      3,11  ,  3,12  , 
      4,11  ,  4,12  , 
      5,11  ,  5,12   , 
      
      // Mid upper left "blue"
      3,3  ,  3,4  , 3,5  ,  3,6  , 
      4,3  ,  4,4  , 4,5  ,  4,6  , 
      5,3  ,  5,4  , 5,5  ,  5,6  , 
      
      // Mid Upper right blue
      3,7  ,  3,8  ,  3,9  ,  3,10  , 
      4,7  ,  4,8  ,  4,9  ,  4,10  , 
      5,7  ,  5,8  ,  5,9  ,  5,10  , 
      
      // Mid left green
      6,1  ,  6,2  , 6,3  , 
      7,1  ,  7,2  , 7,3  , 
      8,1  ,  8,2  , 8,3  , 
      9,1  ,  9,2  , 9,3  , 
      
      // Mid left blue
      6,4  , 6,5  , 
      7,4  , 7,5  , 
      8,4  , 8,5  , 
      9,4  , 9,5  , 
      10,4 , 10,5  , 
      11,4 , 11,5  , 
      
      // Mid mid green
      6,6  , 6,7  , 
      7,6  , 7,7  , 
      8,6  , 8,7  , 
      9,6  , 9,7  , 
      10,6 , 10,7  , 
      11,6 , 11,7  , 
      
      // Mid right blue
      6,8  ,  6,9  , 
      7,8  ,  7,9  , 
      8,8  ,  8,9  , 
      9,8  ,  9,9  , 
      10,8 ,  10,9  , 
      11,8 ,  11,9  , 
      
      // Mid right green
      6,10  ,  6,11  ,  6,12  , 
      7,10  ,  7,11  ,  7,12  , 
      8,10  ,  8,11  ,  8,12  , 
      9,10  ,  9,11  ,  9,12  , 
      
      // Lower left dark green
      10,1  , 10,2  ,  10,3  , 
      11,1  , 11,2  ,  11,3  , 
      12,1  , 12,2  ,  12,3  , 12,4  , 12,5  ,  12,6  , 
      
      // Lower right dark green
      10,10  ,  10,11  , 10,12  , 
      11,10  ,  11,11  , 11,12  , 
      12,7   ,  12,8   ,  12,9  , 12,10  , 12,11  ,  12,12  
      
    };
    IntArgs regions(num_regions*24, _regions);


    // the hints
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        int v = dat[i*n+j];
        rel(*this, x[i*n+j] != 0);
        if (v != X) {
          // X is the unknowns
          rel(*this, x[i*n+j] == v);
        }
      }
    }

    Matrix<IntVarArray> m(x, n, n);
    // rows and columns
    for(int i = 0; i < n; i++) {
      for(int j = -1; j <= 9; j++) {
        count(*this, m.row(i), j, IRT_EQ, occ[j+1], opt.icl()); 
        count(*this, m.col(i), j, IRT_EQ, occ[j+1], opt.icl()); 
      }
    }

    // the regions
    for(int i = 0; i < num_regions; i++) {
      IntVarArgs reg;
      for(int j = i*n; j < (i*n)+12; j++) {
        int r1 = regions[j*2+0]-1;
        int r2 = regions[j*2+1]-1;
        reg << x[r1*n+r2];
      }
      for(int k = -1; k <= 9; k++) {
        count(*this, reg, k, IRT_EQ, occ[k+1], opt.icl()); 
      }
    }
    
    branch(*this, x, INT_VAR_DEGREE_MAX(), INT_VAL_MED());

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
        int v = x[i*n+j].val();
        if (v == P) {
          // Pi
          os << "P" << " ";
        } else {
          os << v << " ";
        }
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

