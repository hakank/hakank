/*

  KenKen puzzle in Gecode.

  http://en.wikipedia.org/wiki/KenKen
  """
  KenKen or KEN-KEN is a style of arithmetic and logical puzzle sharing 
  several characteristics with sudoku. The name comes from Japanese and 
  is translated as "square wisdom" or "cleverness squared".
  ...
  The objective is to fill the grid in with the digits 1 through 6 such that:

    * Each row contains exactly one of each digit
    * Each column contains exactly one of each digit
    * Each bold-outlined group of cells is a cage containing digits which 
      achieve the specified result using the specified mathematical operation: 
        addition (+), 
        subtraction (-), 
        multiplication (x), 
        and division (/). 
        (Unlike in Killer sudoku, digits may repeat within a group.)

  ...
  More complex KenKen problems are formed using the principles described 
  above but omitting the symbols +, -, x and /, thus leaving them as 
  yet another unknown to be determined.
  """


  Compare with the following models:
  * Comet   : http://www.hakank.org/comet/kenken2.co
  * MiniZinc: http://www.hakank.org/minizinc/kenken2.mzn
  * SICStus: http://www.hakank.org/sicstus/kenken2.pl
  * ECLiPSe: http://www.hakank.org/eclipse/kenken2.ecl

  This Gecode model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Gecode page: http://www.hakank.org/gecode/

 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include "gecode/minimodel.hh"

using namespace Gecode;

using std::cout;
using std::endl;

void prod(Space& space, IntVarArgs x, IntVar p, IntConLevel icl = ICL_DOM) {
  
  int n = x.size();
  IntVarArray pp(space, n, 0, 10000);
  rel(space, pp[0] == x[0]);
  for(int i = 1; i < n; i++) {
    rel(space, pp[i] == pp[i-1]*x[i], icl);
  }

  rel(space, p == pp[n-1], icl);

}

class KenKen : public Script {
protected:

  static const int n = 6;

  IntVarArray x;

public:

  // Actual model
  KenKen(const SizeOptions& opt) : 
    x(*this, n*n, 1, n)
  {

    int num_p = 15; //number of segments
    // (max) number of hints per segments
    int num_hints = 4; 
    int _P[] = {
      1,1, 2,1, 0,0, 0,0,   11,
      1,2, 1,3, 0,0, 0,0,   2,
      1,4, 2,4, 0,0, 0,0,  20,
      1,5, 1,6, 2,6, 3,6,   6,
      2,2, 2,3, 0,0, 0,0,   3,
      2,5, 3,5, 0,0, 0,0,   3,
      3,1, 3,2, 4,1, 4,2, 240,
      3,3, 3,4, 0,0, 0,0,   6,
      4,3, 5,3, 0,0, 0,0,   6,
      4,4, 5,4, 5,5, 0,0,   7,
      4,5, 4,6, 0,0, 0,0,  30,
      5,1, 5,2, 0,0, 0,0,   6,
      5,6, 6,6, 0,0, 0,0,   9,
      6,1, 6,2, 6,3, 0,0,   8,
      6,4, 6,5, 0,0, 0,0,   2
    };
    IntArgs P(num_p*(2*num_hints+1), _P);

    Matrix<IntVarArray> m(x, n, n);
    for(int i = 0; i < n; i++) {
      distinct(*this, m.row(i), opt.icl());
      distinct(*this, m.col(i), opt.icl());
    }

    for(int p = 0; p < num_p; p++) {
      IntVarArgs p_tmp;
      int num_num = 0; // number of hints

      for(int i = 0; i < num_hints; i++) {
        int p_test = P[p*9 + 2*i+0];
        if (p_test > 0) {
          int p1 = P[p*9+2*i+0]-1;
          int p2 = P[p*9+2*i+1]-1;
          p_tmp << expr(*this, x[p1*n+p2]);
          num_num++;
        }
      }
      int p_res = P[p*9+2*num_hints];

      // for two hints we may have 
      //    +, -, *, /
      if (num_num == 2) {
        int p1 = P[p*9+0]-1;
        int p2 = P[p*9+1]-1;
        int p3 = P[p*9+2]-1;
        int p4 = P[p*9+3]-1;
        IntVar a = x[p1*n+p2];
        IntVar b = x[p3*n+p4];

        rel(*this, 
            (a + b == p_res) ||
            (a * b == p_res) ||
            (a * p_res == b) ||
            (b * p_res == a) ||
            (a - b == p_res) ||
            (b - a == p_res),
            opt.icl()
            );

      } else {

        //
        // for 3 or 4 hints we have just + or *
        //

        // either sum
        BoolVar is_sum = expr(*this, sum(p_tmp) == p_res, opt.icl());

        // or product
        IntVar pp(*this, 0, 10000);
        prod(*this, p_tmp, pp, opt.icl());
        BoolVar is_prod = expr(*this, pp == p_res, opt.icl());

        rel(*this, is_sum || is_prod);
      }

    }


    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Constructor for cloning s
  KenKen(bool share, KenKen& s) : Script(share,s) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new KenKen(share,*this);
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
  SizeOptions opt("KenKen");
  opt.solutions(0);
  opt.icl(ICL_DOM);

  opt.parse(argc,argv);

  Script::run<KenKen,DFS,SizeOptions>(opt);

  return 0;
}

