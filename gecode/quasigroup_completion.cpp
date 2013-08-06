/*

  Quasigroup completion problem in Gecode.
  
  See: Carla P. Gomes and David Shmoys:
  "Completing Quasigroups or Latin Squares: Structured Graph Coloring Problem"
 
  
  See also
  Ivars Peterson "Completing Latin Squares"
  http://www.maa.org/mathland/mathtrek_5_8_00.html
  """
  Using only the numbers 1, 2, 3, and 4, arrange four sets of these numbers into 
  a four-by-four array so that no column or row contains the same two numbers. 
  The result is known as a Latin square.
  ...
  The so-called quasigroup completion problem concerns a table that is correctly 
  but only partially filled in. The question is whether the remaining blanks in 
  the table can be filled in to obtain a complete Latin square (or a proper 
  quasigroup multiplication table).
  """
  

  Also see the following models:
  MiniZinc: http://www.hakank.org/minizinc/quasigroup_completion.mzn
  Comet   : http://www.hakank.org/comet/quasigroup_completion.co
  JaCoP   : http://www.hakank.org/JaCoP/QuasigroupCompletion.java
  Choco   : http://www.hakank.org/choco/QuasigroupCompletion.java
  Gecode/R: http://www.hakank.org/gecode_r/quasigroup_completion.rb

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .


*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


class Quasigroup : public Script {
protected:

  IntVarArray x; // array version of the square
  // static const int n = 5;
  // static const int n = 10;
  static const int n = 4;

public:

;

  Quasigroup(const Options& opt) 
    : 
    x(*this, n*n, 1, n)
  {

    /*
      Example from Ruben Martins and Ines Lynce
      Breaking Local Symmetries in Quasigroup Completion Problems, page 3
      The solution is unique:
      1 3 2 5 4
      2 5 4 1 3
      4 1 3 2 5
      5 4 1 3 2
      3 2 5 4 1
    */
    /*
    int _m[] = 
    {
     1, 0, 0, 0, 4,
     0, 5, 0, 0, 0,
     4, 0, 0, 2, 0,
     0, 4, 0, 0, 0,
     0, 0, 5, 0, 1
    };
    */

    /*
      Problem from http://www.cs.cornell.edu/gomes/QUASIdemo.html
      (n = 10)
      Pattern #3. 
      Coding:
         dark red   = 1
         light blue = 2 
         dark blue  = 3 
         light red  = 4
         brown      = 5
         green      = 6
         pink       = 7
         grey       = 8
         black      = 9
         yellow     = 10    
      There are 40944 solutions for this pattern.
    */
    /*
    int _m[] = {
       0, 0, 1, 5, 2, 6, 7, 8, 0, 0,
       0, 1, 5, 2, 0, 0, 6, 7, 8, 0,
       1, 5, 2, 0, 0, 0, 0, 6, 7, 8,
       5, 2, 0, 0, 0, 0, 0, 0, 6, 7,
       2, 0, 0, 0, 0, 0, 0, 0, 0, 6,
       4,10, 0, 0, 0, 0, 0, 0, 3, 9,
       0, 4,10, 0, 0, 0, 0, 3, 9, 0,
       0, 0, 4,10, 0, 0, 3, 9, 0, 0,
       0, 0, 0, 4,10, 3, 9, 0, 0, 0, 
       0, 0, 0, 0, 4, 9, 0, 0, 0, 0
         };
    */

    /*
      Example from Global Constraint Catalogue
      http://www.emn.fr/x-info/sdemasse/gccat/Ck_alldifferent.html
      (global constraint k_alldifferent).
      This problem has 12 solutions.
    */
     int _m[] = {
        1, 0, 0, 0,
        0, 0, 0, 3,
        3, 0, 0, 0,
        0, 0, 0, 1
     };


    Matrix<IntVarArray> m(x, n, n);
    
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
        if(_m[i*n+j] > 0) {
          rel(*this, m(i,j), IRT_EQ, _m[i*n+j], opt.icl());
       }
      }
    }
    
    //
    // constraints
    //
    
    
    // all rows and column are distinct
    // well, that's it.
    for(int i = 0; i < n; i++) {
      distinct(*this, m.row(i), opt.icl());
      distinct(*this, m.col(i), opt.icl());
    }

    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());  // 1 sol: 0 failures
    

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    Matrix<IntVarArray> m(x, n, n);
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
        os.width(3);
        os << m(i,j) << " ";
      }
      os << std::endl;
    }
    os << std::endl;
  }

  // Constructor for cloning s
  Quasigroup(bool share, Quasigroup& s) : Script(share,s) {
    x.update(*this, share, s.x);

  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Quasigroup(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  Options opt("Quasigroup");
  opt.solutions(0);
  opt.icl(ICL_DOM);
  opt.iterations(20000);
  opt.parse(argc,argv);
  Script::run<Quasigroup,DFS,Options>(opt);

  return 0;
}


