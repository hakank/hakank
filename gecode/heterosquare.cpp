/*

  Heterosquare in Gecode.
 
  From http://willow.engr.uconn.edu/cometPubWiki/index.php/Heterosquare
  """
  A heterosquare of order n is a n*n square whose elements are 
  distinct integers from 1 to n^2 such that the sums of the rows, 
  columns and diagonals are all different. Here is an example of 
  heterosquare of order 3 
             19
  
  1  2  3    6
  8  9  4    21
  7  6  5    18
  
  16 17 12   15  (Sums)
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minzinc/heterosquare.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/heterosquare.pl
  * ECLiPSe: http://www.hakank.org/eclipse/heterosquare.ecl


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/


#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/set.hh>


using namespace Gecode;

using std::cout;
using std::endl;
using std::setw;

class Heterosquare : public Script {

protected:

  int n; // size of grid, from opt.size()

  IntVarArray x;  // the grid
  IntVarArray row_sums;
  IntVarArray col_sums;
  IntVar diag1;
  IntVar diag2;

public:

  Heterosquare(const SizeOptions& opt) 
    : 
    n(opt.size()),
    x(*this, n*n, 1, n*n),
    row_sums(*this, n, 1,n*n*n),
    col_sums(*this, n, 1,n*n*n),    
    diag1(*this, 1,n*n*n),
    diag2(*this, 1,n*n*n)
  {

    Matrix<IntVarArray> m(x, n, n);

    // all the entries in the matrix should be different
    distinct(*this, x);

    // and all sums should be different
    IntVarArgs all_sums;
    all_sums << row_sums;
    all_sums << col_sums;
    all_sums << diag1;
    all_sums << diag2;    
    distinct(*this, all_sums);

    // rows/column sums and diagonals
    IntVarArgs v_diag1;
    IntVarArgs v_diag2;
    for(int i = 0; i < n; i++) {
      rel(*this, sum(m.row(i)) == row_sums[i]);
      rel(*this, sum(m.col(i)) == col_sums[i]);
      v_diag1 << m(i,i);
      v_diag2 << m(n-i-1,i);
    }
   
    rel(*this, sum(v_diag1) == diag1);
    rel(*this, sum(v_diag2) == diag2);
   
      
    // branching
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
 
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    Matrix<IntVarArray> m(x, n, n);
    // os << m << endl;

    for(int j = 0; j < n; j++) {
      os << "    ";
    }
    os << "| " << diag2 << " (diag2)" <<  endl;
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        os << setw(3) << m(j,i) << " ";
      }
      os << " = " << setw(3) << row_sums[i] << endl;
    }
    for(int j = 0; j < n; j++) {
      os << "-----";
    }
    os << endl;
    for(int j = 0; j < n; j++) {
      os << setw(3) << col_sums[j] << " ";
    }
    // os << endl;
    os << "| " << diag1 << " (diag1)" << endl;
    os << endl;

  }


  // Constructor for cloning s
  Heterosquare(bool share, Heterosquare& s) : Script(share,s), n(s.n) {
    x.update(*this, share, s.x);
    row_sums.update(*this, share, s.row_sums);
    col_sums.update(*this, share, s.col_sums);
    diag1.update(*this, share, s.diag1);
    diag2.update(*this, share, s.diag2);
    
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Heterosquare(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  SizeOptions opt("Heterosquare");
  opt.solutions(0);
  opt.size(13);

  opt.parse(argc,argv);

  Script::run<Heterosquare,DFS,SizeOptions>(opt);    

  return 0;
}


