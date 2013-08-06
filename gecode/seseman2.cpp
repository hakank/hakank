/*

  Seseman problem in Gecode.

  This is a more general model than seseman.cpp:
  (http://www.hakank.org/gecode/seseman.cpp)

  Description of the problem:
  
  n is the length of a border
  There are (n-2)^2 "holes", i.e.
  there are n^2 - (n-2)^2 variables to find out.
 
  The simplest problem, n = 3 (n x n matrix)
  which is represented by the following matrix:
 
   a b c 
   d   e 
   f g h 
  
  Where the following constraints must hold:
 
    a + b + c = border_sum
    a + d + f = border_sum
    c + e + h = border_sum
    f + g + h = border_sum
    a + b + c + d + e + f = total_sum


  For a (swedish) discussion of this problem, see
  "Sesemans matematiska klosterproblem samt lite Constraint Logic Programming"
  http://www.hakank.org/webblogg/archives/001084.html
  and
  Seseman's Convent Problem: http://www.hakank.org/seseman/seseman.cgi
  (using ECLiPSe code)

  It was also is commented in the (swedish) blog post
  "Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc"
  http://www.hakank.org/webblogg/archives/001209.html
  

  Also compare with other models:
  - MiniZinc: http://www.hakank.org/minizinc/seseman.mzn
  - MiniZinc: http://www.hakank.org/minizinc/seseman2.mzn (generalized model)
  - Comet:    http://www.hakank.org/comet/seseman.co
  - JaCoP:    http://www.hakank.org/JaCoP/Seseman.java
  - Choco:    http://www.hakank.org/Choco/Seseman.java
  - Gecode/R: http://www.hakank.org/gecode_r/seseman.rb
  - Excel/OpenOffice Scalc: http://www.hakank.org/oocalc_excel/seseman.xls


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

class Seseman : public Script {
protected:
  const int n;      // size of square
  IntVarArray x;    // the square (as array)
  IntVar total_sum; // total sum
public:

  Seseman(const SizeOptions& opt) 
    : 
    n(opt.size()), 
    x(*this, n*n, 0, n*n),
    total_sum(*this, 1, (n*n)*(n*n)) {

    // Row sum
    static const int row_sum = n*n;

    // Matrix-wrapper for the square
    Matrix<IntVarArray> m(x, n, n);

    // 0 in all cells in the middle
    for(int i = 1; i < n-1; i++) 
      for(int j = 1; j < n-1; j++) 
        rel(*this, m(i,j), IRT_EQ, 0, opt.icl());

    // only the boundary rows/columns should be summed == row_sum
    // and they should be > 0
    IntVarArgs first_row(n);
    IntVarArgs last_row(n);
    IntVarArgs first_column(n);
    IntVarArgs last_column(n);
    for (int i = 0; i < n; i++) {
      first_row[i]    = m(i,0);
      last_row[i]     = m(i,n-1);
      first_column[i] = m(0,i);
      last_column[i]  = m(n-1,i);
      
      // the summed element must be > 0
      rel(*this, m(i,0),   IRT_GR, 0, opt.icl());
      rel(*this, m(i,n-1), IRT_GR, 0, opt.icl());
      rel(*this, m(0,i),   IRT_GR, 0, opt.icl());
      rel(*this, m(n-1,i), IRT_GR, 0, opt.icl());
      
    }
    // and sum'em up
    linear(*this, first_row,    IRT_EQ, row_sum, opt.icl());
    linear(*this, last_row,     IRT_EQ, row_sum, opt.icl());
    linear(*this, first_column, IRT_EQ, row_sum, opt.icl());
    linear(*this, last_column,  IRT_EQ, row_sum, opt.icl());

    // sum of all elements == total_sum
    linear(*this, x, IRT_EQ, total_sum, opt.icl());

    // branch(*this, x, INT_VAR_NONE, INT_VAL_MIN());
    // This gives a neater search tree:
    // branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_SPLIT_MIN()); 
    // somewhat better: 
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_SPLIT_MAX()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {

    os << "Total sum: " << total_sum << std::endl;
    os << "Row sum: " << n*n << std::endl;

    // Matrix-wrapper for the square
    Matrix<IntVarArray> m(x, n, n);
    os << m << std::endl;
    /*
    for (int i = 0; i < n; i++) {
      os << "\t";
      for (int j = 0; j < n; j++) {
        os.width(2);
        os << m(i,j) << "  ";
      }
      os << std::endl;
    }
    */
    os << std::endl;


  }

  // Constructor for cloning s
  Seseman(bool share, Seseman& s) : Script(share,s), n(s.n) {
    x.update(*this, share, s.x);
    total_sum.update(*this, share, s.total_sum);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Seseman(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  SizeOptions opt("Seseman");
  opt.solutions(1);
  opt.iterations(2000);
  opt.size(3);
  opt.parse(argc,argv);
  Script::run<Seseman,DFS,SizeOptions>(opt);
  return 0;
}


