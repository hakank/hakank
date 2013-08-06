/*
  
  Survo puzzle in Gecode.
  
  http://en.wikipedia.org/wiki/Survo_Puzzle
  """
  Survo puzzle is a kind of logic puzzle presented (in April 2006) and studied 
  by Seppo Mustonen. The name of the puzzle is associated to Mustonen's 
  Survo system which is a general environment for statistical computing and 
  related areas.
  
  In a Survo puzzle the task is to fill an m * n table by integers 1,2,...,m*n so 
  that each of these numbers appears only once and their row and column sums are 
  equal to integers given on the bottom and the right side of the table. 
  Often some of the integers are given readily in the table in order to 
  guarantee uniqueness of the solution and/or for making the task easier.
  """
  
  See also
  http://www.survo.fi/english/index.html
  http://www.survo.fi/puzzles/index.html
 
  References:
  - Mustonen, S. (2006b). "On certain cross sum puzzles"
    http://www.survo.fi/papers/puzzles.pdf 
  - Mustonen, S. (2007b). "Enumeration of uniquely solvable open Survo puzzles." 
    http://www.survo.fi/papers/enum_survo_puzzles.pdf 
  - Kimmo Vehkalahti: "Some comments on magic squares and Survo puzzles" 
    http://www.helsinki.fi/~kvehkala/Kimmo_Vehkalahti_Windsor.pdf
  - R code: http://koti.mbnet.fi/tuimala/tiedostot/survo.R

  See my other models:
  MiniZinc: http://www.hakank.org/minizinc/survo_puzzle.mzn
  Comet: http://www.hakank.org/comet/survo_puzzle.co
  JaCoP: http://www.hakank.org/JaCoP/SurvoPuzzle.java
  Choco: http://www.hakank.org/choco/SurvoPuzzle.java


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

class SurvoPuzzle : public Script {
protected:

  IntVarArray x;
  static const int rows = 3;
  static const int cols = 6;

public:

;

  SurvoPuzzle(const Options& opt) 
    : 
    x(*this, rows*cols, 1, rows*cols)
  {

    /*
       Data from http://www.survo.fi/puzzles/280708.txt, third puzzle
       Survo puzzle 128/2008 (1700) #364-35846
      
          A  B  C  D  E  F
       1  *  *  *  *  *  * 30
       2  *  * 18  *  *  * 86
       3  *  *  *  *  *  * 55
         22 11 42 32 27 37

       Solution:
        4  1 10  5  3  7  = 30
       12  8 18 16 15 17  = 86
        6  2 14 11  9 13  = 55
       -----------------
       22 11 42 32 27 37
    */

    int _rowsums[] = {30, 86, 55};
    int _colsums[] = {22, 11, 42, 32, 27, 37};
    int _m[rows][cols] = 
      {
        {0, 0,  0, 0, 0, 0},
        {0, 0, 18, 0, 0, 0},
        {0, 0,  0, 0, 0, 0}
      };
    
    Matrix<IntVarArray> m(x, cols, rows);
    
    for (int r = 0; r < rows; r++) {
      for (int c = 0; c < cols; c++) {
        if(_m[r][c] > 0) {
          rel(*this, m(c, r), IRT_EQ, _m[r][c], opt.icl());
       }
      }
    }
    
    //
    // constraints
    //
    
    // all different
    distinct(*this, x, opt.icl());  
    
    // sum rows
    for(int r = 0; r < rows; r++) {
      linear(*this, m.row(r), IRT_EQ, _rowsums[r], opt.icl());
    }
    
    // sum cols
    for(int c = 0; c < cols; c++) {
      linear(*this, m.col(c), IRT_EQ, _colsums[c], opt.icl());
    }
    
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_SPLIT_MIN());  // 1 sol: 55 fail all: 169 fail
    

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    Matrix<IntVarArray> m(x, cols, rows);
    for (int r = 0; r < rows; r++) {
      for (int c = 0; c < cols; c++) {
        os.width(3);
        os << m(c, r) << " ";
      }
      os << std::endl;
    }
    os << std::endl;
  }

  // Constructor for cloning s
  SurvoPuzzle(bool share, SurvoPuzzle& s) : Script(share,s) {
    x.update(*this, share, s.x);

  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new SurvoPuzzle(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  Options opt("SurvoPuzzle");
  opt.solutions(0);
  opt.icl(ICL_DOM);
  opt.iterations(20000);
  opt.parse(argc,argv);
  Script::run<SurvoPuzzle,DFS,Options>(opt);

  return 0;
}


