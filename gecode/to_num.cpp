/*

  to_num function in Gecode.

  This model simply use the function to_num which channels an integer 
  to an array of integers in a specific base.

  Also see:
  Comet: http://www.hakank.org/comet/toNum.co

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


//
// to_num(x, z, base, icl):
// 
// simple channeling between 
//   - an IntVarArray x of digits (in base 'base')
//   - an IntVar z.
//
// It is the responsibility of the user to assure that
// the domain of x is correct.
// 
void to_num(Space & space, IntVarArray x, IntVar z, int base = 10, IntConLevel icl = ICL_BND) {
  
  int len = x.size();
  IntArgs coeffs(len);
  for(int r = 0; r < len; r++) {
    coeffs[r] = pow(base, len-r-1);
  }
  
  // linear(space, coeffs, x, IRT_EQ, z, icl);
  rel(space, sum(coeffs, x) == z, icl);
}



class ToNum : public Script {
protected:

  static const int arr_len = 6;      // length of array
  int base;  // base to use

  IntVar z;                    // the digit
  IntVarArray x;               // array version of the digit z

public:

  ToNum(const SizeOptions& opt) 
  : 
    base(opt.size()),
    z(*this, 0, Int::Limits::max),
    x(*this, arr_len, 0, base-1)
  {

    IntArgs coeffs(arr_len);
    for(int r = 0; r < arr_len; r++) {
      coeffs[r] = pow(base, arr_len-r-1);
    }

    distinct(*this, x, opt.icl());
    // rel(*this, z == 654321, opt.icl());
    to_num(*this, x, z, base, opt.icl());

    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_SPLIT_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << ": z: " << z << " base: " << base << std::endl;
    os << std::endl;
  }

  // Constructor for cloning s
  ToNum(bool share, ToNum& s) : Script(share,s),
                                base(s.base) {
    x.update(*this, share, s.x);
    z.update(*this, share, s.z);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new ToNum(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  //
  // loops over different bases (as opt.size())
  //
  // for(int base = 6; base < 20; base++) {
  // std::cout << "base: " << base << std::endl;
    SizeOptions opt("ToNum");
    // opt.size(base); 
    // opt.solutions(2);
    opt.parse(argc,argv);
    if (!opt.size()) 
      opt.size(10);
    // std::cout << "#solutions: " << opt.solutions() << std::endl;
    Script::run<ToNum,DFS,SizeOptions>(opt);
    // }
    return 0;
}


