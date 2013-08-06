/*

  Kakuro puzzle in Gecode.

  http://en.wikipedia.org/wiki/Kakuro
  """
  The object of the puzzle is to insert a digit from 1 to 9 inclusive 
  into each white cell such that the sum of the numbers in each entry 
  matches the clue associated with it and that no digit is duplicated in 
  any entry. It is that lack of duplication that makes creating Kakuro 
  puzzles with unique solutions possible, and which means solving a Kakuro 
  puzzle involves investigating combinations more, compared to Sudoku in 
  which the focus is on permutations. There is an unwritten rule for 
  making Kakuro puzzles that each clue must have at least two numbers 
  that add up to it. This is because including one number is mathematically 
  trivial when solving Kakuro puzzles; one can simply disregard the 
  number entirely and subtract it from the clue it indicates.
  """

  This model solves the problem at the Wikipedia page. 
  For a larger picture, see
  http://en.wikipedia.org/wiki/File:Kakuro_black_box.svg

  The solution:
    9 7 0 0 8 7 9
    8 9 0 8 9 5 7
    6 8 5 9 7 0 0
    0 6 1 0 2 6 0
    0 0 4 6 1 3 2
    8 9 3 1 0 1 4
    3 1 2 0 0 2 1

  or rather

    9 7 . . 8 7 9
    8 9 _ 8 9 5 7
    6 8 5 9 7 . .
    . 6 1 . 2 6 .
    . . 4 6 1 3 2
    8 9 3 1 . 1 4
    3 1 2 . . 2 1


  Compare with the following models:
  * Comet   : http://www.hakank.org/comet/kakuro.co
  * MiniZinc: http://www.hakank.org/minizinc/kakuro.mzn
  * SICStus: http://www.hakank.org/sicstus/kakuro.pl
  * ECLiPSe: http://www.hakank.org/eclipse/kakuro.ecl

  This Gecode model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Gecode page: http://www.hakank.org/gecode/

 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include "gecode/minimodel.hh"

using namespace Gecode;

using std::cout;
using std::endl;

class Kakuro : public Script {
protected:

  static const int n = 7;

  IntVarArray x;

public:

  // Actual model
  Kakuro(const SizeOptions& opt) : 
    x(*this, n*n, 0, 9)
  {

    int num_p = 24; //number of segments
    // (max) number of hints per segments
    int num_hints = 5; 
    int _P[] = {
      1,1, 1,2, 0,0, 0,0, 0,0,  16,
      1,5, 1,6, 1,7, 0,0, 0,0,  24,
      2,1, 2,2, 0,0, 0,0, 0,0,  17,
      2,4, 2,5, 2,6, 2,7, 0,0,  29,
      3,1, 3,2, 3,3, 3,4, 3,5,  35,
      4,2, 4,3, 0,0, 0,0, 0,0,   7, 
      4,5, 4,6, 0,0, 0,0, 0,0,   8, 
      5,3, 5,4, 5,5, 5,6, 5,7,  16, 
      6,1, 6,2, 6,3, 6,4, 0,0,  21, 
      6,6, 6,7, 0,0, 0,0, 0,0,   5, 
      7,1, 7,2, 7,3, 0,0, 0,0,   6, 
      7,6, 7,7, 0,0, 0,0, 0,0,   3, 
      
      1,1, 2,1, 3,1, 0,0, 0,0,  23, 
      1,2, 2,2, 3,2, 4,2, 0,0,  30, 
      1,5, 2,5, 3,5, 4,5, 5,5,  27, 
      1,6, 2,6, 0,0, 0,0, 0,0,  12, 
      1,7, 2,7, 0,0, 0,0, 0,0,  16, 
      2,4, 3,4, 0,0, 0,0, 0,0,  17,    
      3,3, 4,3, 5,3, 6,3, 7,3,  15, 
      4,6, 5,6, 6,6, 7,6, 0,0,  12, 
      5,4, 6,4, 0,0, 0,0, 0,0,   7,    
      5,7, 6,7, 7,7, 0,0, 0,0,   7, 
      6,1, 7,1, 0,0, 0,0, 0,0,  11, 
      6,2, 7,2, 0,0, 0,0, 0,0,  10
    };

    IntArgs P(num_p*(2*num_hints+1), _P);

    // 
    // the blanks, coded as 0 in x
    //
    int num_blanks = 13;
    int _blanks[] = 
      {
        1,3, 1,4,
        2,3,
        3,6, 3,7,
        4,1, 4,4, 4,7,
        5,1, 5,2,
        6,5,
        7,4, 7,5
      };
    IntArgs blanks(2*num_blanks, _blanks);

    for(int i = 0; i < num_blanks; i++) {
      int b1 = blanks[2*i+0]-1;
      int b2 = blanks[2*i+1]-1;
      rel(*this, x[b1*n+b2] == 0);
    }

    for(int p = 0; p < num_p; p++) {
      IntVarArgs p_tmp;
      for(int i = 0; i < num_hints; i++) {
        int p_test = P[p*11 + 2*i+0];
        if (p_test > 0) {
          int p1 = P[p*11+2*i+0]-1;
          int p2 = P[p*11+2*i+1]-1;
          p_tmp << expr(*this, x[p1*n+p2]);

          rel(*this, x[p1*n+p2] > 0);
        }
      }
      int p_sum = P[p*11+2*num_hints];
      rel(*this, sum(p_tmp) == p_sum, opt.icl());

      distinct(*this, p_tmp);
    }


    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Constructor for cloning s
  Kakuro(bool share, Kakuro& s) : Script(share,s) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Kakuro(share,*this);
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        if (x[i*n+j].val() > 0) {
          os << x[i*n+j] << " ";   
        } else {
          os << ".  ";
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
  SizeOptions opt("Kakuro");
  opt.solutions(0);
  opt.icl(ICL_DOM);

  opt.parse(argc,argv);

  Script::run<Kakuro,DFS,SizeOptions>(opt);

  return 0;
}

