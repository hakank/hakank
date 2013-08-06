/*

  Seseman problem in Gecode.

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
  (using Eclipse code)

  It was also is commented in the (swedish) blog post
  "Constraint Programming: Minizinc, Gecode/flatzinc och ECLiPSe/minizinc"
  http://www.hakank.org/webblogg/archives/001209.html
  

  Also compare with other models:
  - MiniZinc: http://www.hakank.org/minizinc/seseman.mzn
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
  static const int size = 8;       // number of letters
  static const int row_sum = 9;    // row sum
  static const int total_sum = 24; // total sum

  IntVarArray x;
public:

  Seseman(const Options& opt) : x(*this, size, 1, row_sum) {
    IntVar
      a(x[0]), b(x[1]), c(x[2]), d(x[3]),
      e(x[4]), f(x[5]), g(x[6]), h(x[7]);

    rel(*this, a+b+c == row_sum, opt.icl());
    rel(*this, a+d+f == row_sum, opt.icl());
    rel(*this, c+e+h == row_sum, opt.icl());
    rel(*this, f+g+h == row_sum, opt.icl());
    rel(*this, a+b+c+d+e+f+g+h == total_sum, opt.icl());

    // branch(*this, x, INT_VAR_NONE(), INT_VAL_MIN());
    // This gives a neater search tree:
    // branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_SPLIT_MIN()); 
    // somewhat better: 
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_SPLIT_MAX()); 

  }
  /// Print solution
  virtual void
  print(std::ostream& os) const {
    // os << "	" << x << std::endl;
    os << x[0] << " " << x[1] << " " << x[2] << std::endl;
    os << x[3] << " " << " "  << " " << x[4] << std::endl;
    os << x[5] << " " << x[6] << " " << x[7] << std::endl;
    os << std::endl;
  }

  /// Constructor for cloning s
  Seseman(bool share, Seseman& s) : Script(share,s) {
    x.update(*this, share, s.x);
  }

  /// Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Seseman(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  Options opt("Seseman");
  opt.solutions(0);
  opt.iterations(20000);
  opt.parse(argc,argv);
  Script::run<Seseman,DFS,Options>(opt);
  return 0;
}


