/*
  
  Olympic puzzle in Gecode.

  Benchmark for Prolog (BProlog)
  """
    File   : olympic.pl
    Author : Neng-Fa ZHOU
    Date   : 1993

    Purpose: solve a puzzle taken from Olympic Arithmetic Contest

     Given ten variables with the following configuration:

                 X7   X8   X9   X10

                    X4   X5   X6

                       X2   X1             

                          X1

    We already know that X1 is equal to 3 and want to assign each variable
    with a different integer from {1,2,...,10} such that for any three
    variables 
                        Xi   Xj

                           Xk
    the following constraint is satisfied:

                      |Xi-Xj| = Xk
  """


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/olympic.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/olympic.ecl
  * ECLiPSe: http://www.hakank.org/eclipse/olympic.ecl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;
using std::setw;
using std::string;


class Olympic : public Script {
protected:

  static const int n = 10;

  IntVarArray Vars;

public:

  void minus(Space &space, IntVar x, IntVar y, IntVar z) {
    rel(space, z == abs(x-y));
  }

  Olympic(const Options& opt) 
    : 
    Vars(*this, n, 1, n)
  {

    IntVar
      X1(Vars[0]),
      X2(Vars[1]),
      X3(Vars[2]),
      X4(Vars[3]),
      X5(Vars[4]),
      X6(Vars[5]),
      X7(Vars[6]),
      X8(Vars[7]),
      X9(Vars[8]),
      X10(Vars[9]);

    distinct(*this, Vars, opt.icl());

    rel(*this, X1 == 3);
    minus(*this, X2,X3,X1);
    minus(*this, X4,X5,X2);
    minus(*this, X5,X6,X3);
    minus(*this, X7,X8,X4);
    minus(*this, X8,X9,X5);
    minus(*this, X9,X10,X6);


    // branching
    branch(*this, Vars, INT_VAR_NONE(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "Vars: " << Vars << endl;
    os << endl;

  }


  // Constructor for cloning s
  Olympic(bool share, Olympic& s) : Script(share,s) {
    Vars.update(*this, share, s.Vars);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Olympic(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("Olympic");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<Olympic,DFS,Options>(opt);
    
  return 0;
}


