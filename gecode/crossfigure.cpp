/*

  Crossfigure problem in Gecode.

  CSPLib problem 21
  http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob021/index.html
  """
  Crossfigures are the numerical equivalent of crosswords. You have a grid and some 
  clues with numerical answers to place on this grid. Clues come in several different 
  forms (for example: Across 1. 25 across times two, 2. five dozen, 5. a square number, 
  10. prime, 14. 29 across times 21 down ...). 
  """
 
  Also, see 
  http://en.wikipedia.org/wiki/Cross-figure
  
  William Y. Sit: "On Crossnumber Puzzles and The Lucas-Bonaccio Farm 1998
  http://scisun.sci.ccny.cuny.edu/~wyscc/CrossNumber.pdf
  
  Bill Williams: Crossnumber Puzzle, The Little Pigley Farm
  http://jig.joelpomerantz.com/fun/dogsmead.html

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/crossfigure.mzn
  * Comet   : http://www.hakank.org/comet/crossfigure.co

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;

void to_num(Space & space, IntVarArray x, IntVar z, int base = 10, IntConLevel icl = ICL_BND) {
  
  int len = x.size();
  IntArgs coeffs(len);
  for(int r = 0; r < len; r++) {
    coeffs[r] = pow(base, len-r-1);
  }
  
  // linear(space, coeffs, x, IRT_EQ, z, icl);
  rel(space, sum(coeffs, x) == z, icl);
}


/*
 across(Matrix, Across, Len, Row, Col)
	Constrains 'Across' to be equal to the number represented by the
	'Len' digits starting at position (Row, Col) of the array 'Matrix'
	and proceeding across.
*/
void across(Space& space, IntVarArray matrix, IntVar Across, int Len, int Row, int Col, int n) {
  IntVarArray tmp(space, Len, 0, 9999);
  to_num(space, tmp, Across);
  for(int i = 0; i < Len; i++) {
    // also, convert to 0-based
    rel(space, matrix[(Row-1)*n+(Col-1)+i] == tmp[i]);
  }
}

/*
 down(Matrix, Down, Len, Row, Col):
	Constrains 'Down' to be equal to the number represented by the
	'Len' digits starting at position (Row, Col) of the array 'Matrix'
	and proceeding down.
*/
void down(Space& space, IntVarArray matrix, IntVar Down, int Len, int Row, int Col, int n) {
 
  IntVarArray tmp(space, Len, 0, 9999);
  to_num(space, tmp, Down);
  for(int i = 0; i < Len; i++) {
    // also, convert to 0-based
    rel(space, matrix[(Row-1+i)*n+Col-1] == tmp[i]);
  }
}

// is a square?
void is_square(Space& space, IntVar x) {
  IntVar y(space, 0, 100);
  rel(space, y*y == x);
}

// is a prime?
void is_prime(Space& space, IntVar x) {
  for(int i = 2; i <= 99; i++) {
    rel(space, (i < x) >> (x % i > 0));
  }
}


// fix the black boxes and convert to 0-based
void fix_box(Space& space, IntVarArray x, int r, int c, int n) {
  rel(space, x[(r-1)*n+(c-1)] == 0);
}


class Crossfigure : public Script {
protected:

  static const int n = 9; // size of matrix

  // matrix
  IntVarArray x; 

public:

;

  Crossfigure(const SizeOptions& opt) 
    : 
    x(*this, n*n, 0, 9)
  {

    IntVar A1(*this, 0, 9999);
    IntVar A4(*this, 0, 9999);
    IntVar A7(*this, 0, 9999);
    IntVar A8(*this, 0, 9999);
    IntVar A9(*this, 0, 9999);
    IntVar A10(*this, 0, 9999);
    IntVar A11(*this, 0, 9999);
    IntVar A13(*this, 0, 9999);
    IntVar A15(*this, 0, 9999);
    IntVar A17(*this, 0, 9999);
    IntVar A20(*this, 0, 9999);
    IntVar A23(*this, 0, 9999);
    IntVar A24(*this, 0, 9999);
    IntVar A25(*this, 0, 9999);
    IntVar A27(*this, 0, 9999);
    IntVar A28(*this, 0, 9999);
    IntVar A29(*this, 0, 9999);
    IntVar A30(*this, 0, 9999);
    
    IntVar D1(*this, 0, 9999);
    IntVar D2(*this, 0, 9999);
    IntVar D3(*this, 0, 9999);
    IntVar D4(*this, 0, 9999);
    IntVar D5(*this, 0, 9999);
    IntVar D6(*this, 0, 9999);
    IntVar D10(*this, 0, 9999);
    IntVar D12(*this, 0, 9999);
    IntVar D14(*this, 0, 9999);
    IntVar D16(*this, 0, 9999);
    IntVar D17(*this, 0, 9999);
    IntVar D18(*this, 0, 9999);
    IntVar D19(*this, 0, 9999);
    IntVar D20(*this, 0, 9999);
    IntVar D21(*this, 0, 9999);
    IntVar D22(*this, 0, 9999);
    IntVar D26(*this, 0, 9999);
    IntVar D28(*this, 0, 9999);



    // Set up the constraints between the matrix elements and the
    // clue numbers.
    // Note: these are 1-based and is converted to 0-base in
    // the function
    across(*this, x, A1, 4, 1, 1, n); 
    across(*this, x, A4, 4, 1, 6, n); 
    across(*this, x, A7, 2, 2, 1, n); 
    across(*this, x, A8, 3, 2, 4, n); 
    across(*this, x, A9, 2, 2, 8, n); 
    across(*this, x, A10, 2, 3, 3, n); 
    across(*this, x, A11, 2, 3, 6, n); 
    across(*this, x, A13, 4, 4, 1, n); 
    across(*this, x, A15, 4, 4, 6, n); 
    across(*this, x, A17, 4, 6, 1, n); 
    across(*this, x, A20, 4, 6, 6, n); 
    across(*this, x, A23, 2, 7, 3, n); 
    across(*this, x, A24, 2, 7, 6, n); 
    across(*this, x, A25, 2, 8, 1, n); 
    across(*this, x, A27, 3, 8, 4, n); 
    across(*this, x, A28, 2, 8, 8, n); 
    across(*this, x, A29, 4, 9, 1, n); 
    across(*this, x, A30, 4, 9, 6, n); 
    
    down(*this, x, D1, 4, 1, 1, n); 
    down(*this, x, D2, 2, 1, 2, n); 
    down(*this, x, D3, 4, 1, 4, n); 
    down(*this, x, D4, 4, 1, 6, n); 
    down(*this, x, D5, 2, 1, 8, n); 
    down(*this, x, D6, 4, 1, 9, n); 
    down(*this, x, D10, 2, 3, 3, n); 
    down(*this, x, D12, 2, 3, 7, n); 
    down(*this, x, D14, 3, 4, 2, n); 
    down(*this, x, D16, 3, 4, 8, n); 
    down(*this, x, D17, 4, 6, 1, n); 
    down(*this, x, D18, 2, 6, 3, n); 
    down(*this, x, D19, 4, 6, 4, n); 
    down(*this, x, D20, 4, 6, 6, n); 
    down(*this, x, D21, 2, 6, 7, n); 
    down(*this, x, D22, 4, 6, 9, n); 
    down(*this, x, D26, 2, 8, 2, n); 
    down(*this, x, D28, 2, 8, 8, n); 
    
    // Set up the clue constraints.
    //  Across
    //  1 27 across times two
    //  4 4 down plus seventy-one
    //  7 18 down plus four
    //  8 6 down divided by sixteen
    //  9 2 down minus eighteen
    // 10 Dozen in six gross
    // 11 5 down minus seventy
    // 13 26 down times 23 across
    // 15 6 down minus 350
    // 17 25 across times 23 across
    // 20 A square number
    // 23 A prime number
    // 24 A square number
    // 25 20 across divided by seventeen
    // 27 6 down divided by four
    // 28 Four dozen
    // 29 Seven gross
    // 30 22 down plus 450 
    
    rel(*this, A1 == 2 * A27); 
    rel(*this, A4 == D4 + 71); 
    rel(*this, A7 == D18 + 4); 
    rel(*this, A8 == D6 / 16); 
    rel(*this, A9 == D2 - 18); 
    rel(*this, A10 == 6 * 144 / 12); 
    rel(*this, A11 == D5 - 70); 
    rel(*this, A13 == D26 * A23); 
    rel(*this, A15 == D6 - 350); 
    rel(*this, A17 == A25 * A23); 
    is_square(*this, A20); 
    is_prime(*this, A23);
    is_square(*this, A24); 
    rel(*this, A25 == A20 / 17); 
    rel(*this, A27 == D6 / 4); 
    rel(*this, A28 == 4 * 12); 
    rel(*this, A29 == 7 * 144); 
    rel(*this, A30 == D22 + 450); 

    // Down
    //  1 1 across plus twenty-seven
    //  2 Five dozen
    //  3 30 across plus 888
    //  4 Two times 17 across
    //  5 29 across divided by twelve
    //  6 28 across times 23 across
    // 10 10 across plus four
    // 12 Three times 24 across
    // 14 13 across divided by sixteen
    // 16 28 down times fifteen
    // 17 13 across minus 399
    // 18 29 across divided by eighteen
    // 19 22 down minus ninety-four
    // 20 20 across minus nine
    // 21 25 across minus fifty-two
    // 22 20 down times six
    // 26 Five times 24 across
    // 28 21 down plus twenty-seven 
    
    rel(*this, D1 == A1 + 27); 
    rel(*this, D2 == 5 * 12); 
    rel(*this, D3 == A30 + 888); 
    rel(*this, D4 == 2 * A17); 
    rel(*this, D5 == A29 / 12); 
    rel(*this, D6 == A28 * A23); 
    rel(*this, D10 == A10 + 4); 
    rel(*this, D12 == A24 * 3); 
    rel(*this, D14 == A13 / 16); 
    rel(*this, D16 == 15 * D28); 
    rel(*this, D17 == A13 - 399); 
    rel(*this, D18 == A29 / 18); 
    rel(*this, D19 == D22 - 94); 
    rel(*this, D20 == A20 - 9); 
    rel(*this, D21 == A25 - 52); 
    rel(*this, D22 == 6 * D20); 
    rel(*this, D26 == 5 * A24); 
    rel(*this, D28 == D21 + 27);


    // Fix the blackboxes
    // Note: these are 1-based and is converted to 0-base in
    // the function
    fix_box(*this, x,1,5,n);
    fix_box(*this, x,2,3,n);
    fix_box(*this, x,2,7,n);
    fix_box(*this, x,3,2,n);
    fix_box(*this, x,3,5,n);
    fix_box(*this, x,3,8,n);
    fix_box(*this, x,4,5,n);
    fix_box(*this, x,5,1,n);
    fix_box(*this, x,5,3,n);
    fix_box(*this, x,5,4,n);
    fix_box(*this, x,5,5,n);
    fix_box(*this, x,5,6,n);
    fix_box(*this, x,5,7,n);
    fix_box(*this, x,5,9,n);
    fix_box(*this, x,6,5,n);
    fix_box(*this, x,7,2,n);
    fix_box(*this, x,7,5,n);
    fix_box(*this, x,7,8,n);
    fix_box(*this, x,8,3,n);
    fix_box(*this, x,8,7,n);
    fix_box(*this, x,9,5,n);
    

    // branching
    branch(*this, x, INT_VAR_DEGREE_MIN(), INT_VAL_MAX()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    for(int r = 0; r < n; r++) {
      for(int c = 0; c < n; c++) {
        int t = x[r*n+c].val();
        if (t != 0) {
          os << t;
        } else {
          os << " ";
        }
      }
      os << std::endl;
    }
    os << std::endl;
  }

  // Constructor for cloning s
  Crossfigure(bool share, Crossfigure& s) : Script(share,s) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Crossfigure(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  SizeOptions opt("Crossfigure");
  opt.solutions(0);
  opt.parse(argc,argv);
  Script::run<Crossfigure,DFS,SizeOptions>(opt);

  return 0;
}


