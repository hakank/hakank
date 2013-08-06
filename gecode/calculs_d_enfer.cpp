/*

  Calculs d'enfer puzzle in Gecode.
  
  Problem from Jianyang Zhou "The Manual of NCL version 1.2", page 33
  http://citeseer.ist.psu.edu/161721.html
  
  The solution is the manual is:
  """
  a = -16, b = -14, c = -13, d = -12, e = -10,
  f = 4, g = 13, h = -1, i = -3, j = -11, k = -9,
  l = 16, m = -8, n = 11, o = 0, p = -6, q = -4,
  r = 15, s = 2, t = 9, u = -15, v = 14, w = -7,
  x = 7, y = -2, z = -5.
 
  max_{#1\in [1,26]}{|x_{#1}|} minimized to 16
  """
 
  Also, see the discussion of the Z model:
  http://www.comp.rgu.ac.uk/staff/ha/ZCSP/additional_problems/calculs_enfer/calculs_enfer.ps
  which shows the same solution.

  Which is also the solution shown in this model.


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/calculs_d_enfer.mzn
  * Comet   : http://www.hakank.org/comet/calculs_d_enfer.co

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include "gecode/minimodel.hh"

using namespace Gecode;

class Calculs : public MinimizeScript {
protected:

  // Number of letters
  static const int N = 26;

  // Array of letters
  IntVarArray X;

  // The objective is to minimize the maximum of the absolute 
  // values of X[i]
  IntVarArray X_abs;
  IntVar x_max; // The value to minimize

  // For showing all solutions 
  // (if there is an "size" argument to the program)
  int show_all;

public:

  // search engines
  enum {
    SEARCH_DFS,
    SEARCH_BAB,
  };


  /// Actual model
  Calculs(const SizeOptions& opt) : 
    X(*this, N, -N, N),
    X_abs(*this, N, 0, N),
    x_max(*this, 0, N),
    show_all(opt.size())
  {

    IntVar
      a(X[ 0]), b(X[ 1]), c(X[ 2]), e(X[ 4]), f(X[ 5]),
      g(X[ 6]), h(X[ 7]), i(X[ 8]), j(X[ 9]), k(X[10]),
      l(X[11]), m(X[12]), n(X[13]), o(X[14]), p(X[15]),
      q(X[16]), r(X[17]), s(X[18]), t(X[19]), u(X[20]),
      v(X[21]), w(X[22]), x(X[23]), y(X[24]), z(X[25]);


    // x_max is the max value of abs(X)   
    for(int i = 0; i < N; i++)  {
      abs(*this, X[i], X_abs[i], opt.icl());
    }
    max(*this, X_abs, x_max, opt.icl());

    rel(*this, z+e+r+o     == 0, opt.icl());
    rel(*this, o+n+e       == 1, opt.icl());
    rel(*this, t+w+o       == 2, opt.icl());
    rel(*this, t+h+r+e+e   == 3, opt.icl());
    rel(*this, f+o+u+r     == 4, opt.icl());
    rel(*this, f+i+v+e     == 5, opt.icl());
    rel(*this, s+i+x       == 6, opt.icl());
    rel(*this, s+e+v+e+n   == 7, opt.icl());
    rel(*this, e+i+g+h+t   == 8, opt.icl());
    rel(*this, n+i+n+e     == 9, opt.icl());
    rel(*this, t+e+n       == 10, opt.icl());
    rel(*this, e+l+e+v+e+n == 11, opt.icl());
    rel(*this, t+w+e+l+f   == 12, opt.icl());

    distinct(*this, X, opt.icl());

    // for showing all solutions (there are many)
    if (show_all) {
      rel(*this, x_max == 16, opt.icl());
    }

    branch(*this, X, INT_VAR_DEGREE_MAX(), INT_VAL_MIN());

  }

  // Return cost
  virtual IntVar cost(void) const {
    return x_max;
  }



  // Constructor for cloning s
  Calculs(bool share, Calculs& s) : MinimizeScript(share,s), 
                                    show_all(s.show_all) {
    X.update(*this, share, s.X);
    x_max.update(*this, share, s.x_max);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Calculs(share,*this);
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    // (This was inspired by the Gecode example alpha.cpp)
    os << "x_max: " << x_max << std::endl;
    os << "\t";
    for (int i = 0; i < N; i++) {
      os << ((char) (i+'a')) << '=';
      os.width(3);
      os << X[i] << ((i<N-1)?", ":"\n");
      if ((i+1) % 8 == 0)
        os << std::endl << "\t";
    }
    os << std::endl;
  }

};

/** 
 *  main
 */
int
main(int argc, char* argv[]) {
  SizeOptions opt("Calculs");
  opt.solutions(0);
  opt.iterations(10);

  opt.search(Calculs::SEARCH_BAB);
  opt.search(Calculs::SEARCH_DFS, "dfs");
  opt.search(Calculs::SEARCH_BAB, "bab");

  opt.parse(argc,argv);

  // Show all solutions if a program argument,
  if (opt.size()) {
    opt.search(Calculs::SEARCH_DFS);
  }

  if (opt.search() == Calculs::SEARCH_DFS) {
    MinimizeScript::run<Calculs,DFS,SizeOptions>(opt);
  } else {
    MinimizeScript::run<Calculs,BAB,SizeOptions>(opt);
  }

  return 0;
}


