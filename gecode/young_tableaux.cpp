/*

  Young tableaux in Gecode.

  See 
  http://mathworld.wolfram.com/YoungTableau.html
  and
  http://en.wikipedia.org/wiki/Young_tableau
  """
  The partitions of 4 are
   {4}, {3,1}, {2,2}, {2,1,1}, {1,1,1,1}
 
  And the corresponding standard Young tableaux are:
 
  1.   1 2 3 4
 
  2.   1 2 3         1 2 4    1 3 4
       4             3        2
 
  3.   1 2           1 3
       3 4           2 4
 
  4    1 2           1 3      1 4 
       3             2        2 
       4             4        3
 
  5.   1
       2
       3
       4
  """  
  
  Also, compare with the following models:
  - MiniZinc: http://www.hakank.org/minizinc/young_tableaux.mzn
  - Comet: http://www.hakank.org/comet/young_tableuax.co
  - JaCoP: http://www.hakank.org/JaCoP/YoungTableuax.java
  - Choco: http://www.hakank.org/choco/YoungTableuax.java


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


class YoungTableaux : public Script {
protected:

  int n;          // number to create a Young tableaux for
  IntVarArray x;  // array of elements
                  // The empty cells in x are represented as the 
                  // number n+1 for simplifying the
                  // comparisons. In the output n+1 is 
                  // replaced with " ".

  IntVarArray p;  // partition structure


public:

  // Actual model
  YoungTableaux(const SizeOptions& opt) : 
    n(opt.size()),
    x(*this, n*n, 1, n+1),
    p(*this, n, 0, n+1)
  {
    
    std::cout << "n: " << n << std::endl;

    Matrix<IntVarArray> m(x, n, n);

    //
    // 1..n is used exactly once (0 may be used many times)
    //
    for(int i = 0; i < n; i++) {
      count(*this, x, i+1, IRT_EQ, 1, opt.icl());
    }
    
    // always start with 1 in upper left corner
    rel(*this, m(0,0) == 1, opt.icl());

    /*
    // row wise: all rows should be increasing
    for(int i = 0; i < n; i++) {
      for(int j = 1; j < n; j++) {
        rel(*this, m(j,i) >= m(j-1, i), opt.icl());
      }
    }
    
    // column wise: all columns should be increasing
    for(int j = 0; j < n; j++) {
      for(int i = 1; i < n;  i++) {
        rel(*this, m(j,i) >= m(j, i-1), opt.icl());
      }
    }
    */

    // More succint variant of the two constraints above:
    // all rows and columns should be increasing
    for(int i = 0; i < n; i++) {
      rel(*this, m.row(i),IRT_LQ);
      rel(*this, m.col(i),IRT_LQ);
    }

    // calculate the partition
    for(int i = 0; i < n; i++) {
      // p[i] == sum(j in 1..n) (x[i,j] <= n)

      // count the number of n+1 and then subtract this
      // value from n (kind of backwards)
      IntVar t(*this, 0, n+1);
      count(*this, m.row(i), n+1, IRT_EQ, t, opt.icl());
      rel(*this, p[i] == n-t, opt.icl());

    }
    
    // sum(p) == n
    rel(*this, sum(p) == n, opt.icl());
    
    // p is in decreasing order
    rel(*this, p, IRT_GQ, opt.icl());
    
    
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    branch(*this, p, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    
  }

  // Constructor for cloning s
  YoungTableaux(bool share, YoungTableaux& s) : Script(share,s), n(s.n) {
    x.update(*this, share, s.x);
    p.update(*this, share, s.p);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new YoungTableaux(share,*this);
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    Matrix<IntVarArray> m(x, n, n);
    os << "p: " << p << std::endl;
    os << std::endl;
    for(int i = 0; i < n; i++) {
      if (p[i].val() > 0) {
        for(int j = 0; j < n; j++) {
          if (m(j,i).val() != n+1) {
            os << m(j,i);
          } else {
            os << " ";
          }
          os << " ";
        }
        os << std::endl;
      }
    }
    os << std::endl;
  }

};

/** 
 *  main
 */
int
main(int argc, char* argv[]) {
  SizeOptions opt("YoungTableaux");
  opt.solutions(0);
  opt.parse(argc,argv);

  if (!opt.size()) 
    opt.size(4);

  Script::run<YoungTableaux,DFS,SizeOptions>(opt);

  return 0;
}


