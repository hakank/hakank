/*

  Divisible by 9 through 1 problem in Gecode.

  From http://msdn.microsoft.com/en-us/vcsharp/ee957404.aspx
  " Solving Combinatory Problems with LINQ"
  """
  Find a number consisting of 9 digits in which each of the digits 
  from 1 to 9 appears only once. This number must also satisfy these 
  divisibility requirements:
  
   1. The number should be divisible by 9.
   2. If the rightmost digit is removed, the remaining number should 
      be divisible by 8.
   3. If the rightmost digit of the new number is removed, the remaining 
      number should be divisible by 7.
   4. And so on, until there's only one digit (which will necessarily 
      be divisible by 1).
  """
  
  Also, see
  "Intel® Parallel Studio: Great for Serial Code Too (Episode 1)"
  http://software.intel.com/en-us/blogs/2009/12/07/intel-parallel-studio-great-for-serial-code-too-episode-1/

  Compare with the following models:
  - MiniZinc: http://www.hakank.org/minizinc/divisible_by_9_through_1.mzn
  - ECLiPSe: http://www.hakank.org/eclipse/divisible_by_9_through_1.ecl

  Please note that the largest base to use is 10. Larger basis throws 
  "Number out of limits".


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/

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
  linear(space, coeffs, x, IRT_EQ, z, icl);
  // rel(space, sum(coeffs, x) == z, icl); // alternative version
}


class Divisible : public Script {
protected:
  int base;            // base
  IntVarArray digits;  // the digits
  IntVarArray t;       // the numbers. t[0] contains the answer

public:

  Divisible(const SizeOptions& opt) 
    : 
    base(opt.size()), 
    digits(*this, base-1, 1, base-1),
    t(*this, base-1, 0, ceil(pow(base,base-1))-1) {

    int n = base -1;

    // alldifferent digits
    distinct(*this, digits, opt.icl());
   
    // check all divisors
    for(int i = 0; i < n; i++) {
      int m = base-i-1;
      IntVarArray digits_slice(*this, digits.slice(0, 1, m));
      to_num(*this, digits_slice, t[i], base, opt.icl());
      // alternative (more compact):
      // to_num(*this, IntVarArray(*this, digits.slice(0, 1, m)), t[i], base, opt.icl());
      // mod(*this, t[i], IntVar(*this,m,m), IntVar(*this,0,0));
      rel(*this, t[i] % m == 0);
   }
   
    branch(*this, digits, INT_VAR_SIZE_MIN(), INT_VAL_SPLIT_MAX()); 

  }

  // Print the solution
  virtual void
  print(std::ostream& os) const {

    os << "Answer: " << t[0] << std::endl;
    os << "t: " << t << std::endl;
    os << "digits: " << digits << " (in base " << base << ")" << std::endl;
    os << std::endl;

  }

  // Constructor for cloning s
  Divisible(bool share, Divisible& s) : Script(share,s), base(s.base) {
    digits.update(*this, share, s.digits);
    t.update(*this, share, s.t);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Divisible(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  SizeOptions opt("Divisible");

  opt.solutions(0);
  opt.size(10); // the base
  opt.parse(argc,argv);
  Script::run<Divisible,DFS,SizeOptions>(opt);
  return 0;
}


