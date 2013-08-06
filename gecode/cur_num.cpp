/*
  
  Curious numbers puzzle in Gecode.

 """
  Curious Numbers from "Amusements in Mathematics, Dudeney", number 114.

  The number 48 has this peculiarity, that if you add 1 to it the result
  is a square number, and if you add 1 to its half, you also get a
  square number. Now, there is no limit to the numbers that have this
  peculiarity, and it is an interesting puzzle to find three more of
  them---the smallest possible numbers. What are they?
  """ 


  The least such numbers are: 
   [48,49,7,24,25,5],
   [1680,1681,41,840,841,29],
   [57120,57121,239,28560,28561,169], 
   [1940448,1940449,1393,970224,970225,985]


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/cur_num.mzn
  * Comet: http://www.hakank.org/comet/cur_num.co
  * SICStus Prolog: http://www.hakank.org/sicstus/cur_num.pl
  * ECLiPSE: http://www.hakank.org/eclipse/cur_num.ecl


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


class CurNum : public Script {
protected:

  static const int n = 6;

  IntVarArray arr;

public:

  CurNum(const Options& opt) 
    : 
    arr(*this, n, 1, Int::Limits::max)
  {

    IntVar
      X(arr[0]),
      A(arr[1]),
      B(arr[2]),
      C(arr[3]),
      D(arr[4]),
      E(arr[5]);


    rel(*this, 
        X + 1 == A    // if you add 1 to it
        && A == B * B // the result is a square number
        && X == 2 * C // if you to its half
        && C + 1 == D // add 1
        && D == E * E // you also get a square number
        );

    // branching
    branch(*this, arr, INT_VAR_NONE(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << arr[0] << " arr: " << arr << endl;
  }


  // Constructor for cloning s
  CurNum(bool share, CurNum& s) : Script(share,s) {
    arr.update(*this, share, s.arr);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new CurNum(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("CurNum");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<CurNum,DFS,Options>(opt);
    
  return 0;
}


