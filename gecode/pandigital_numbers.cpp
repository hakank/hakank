/*

  Pandigital numbers in Gecode.

  From 
  Albert H. Beiler "Recreations in the Theory of Numbers", quoted from
  http://www.worldofnumbers.com/ninedig1.htm
  """
  [ Chapter VIII : Digits - and the magic of 9 ]
  [ I found the same exposé in Shakuntala Devi's book 
    "Figuring : The Joy of Numbers" ]
 
  The following curious table shows how to arrange the 9 digits so that 
  the product of 2 groups is equal to a number represented by the 
  remaining digits."
 
    12 x 483 = 5796 
    42 x 138 = 5796 
    18 x 297 = 5346 
    27 x 198 = 5346 
    39 x 186 = 7254 
    48 x 159 = 7632 
    28 x 157 = 4396 
    4 x 1738 = 6952 
    4 x 1963 = 7852
  """
 
  See also
  
  * MathWorld http://mathworld.wolfram.com/PandigitalNumber.html
  """
  A number is said to be pandigital if it contains each of the digits 
  from 0 to 9 (and whose leading digit must be nonzero). However, 
  "zeroless" pandigital quantities contain the digits 1 through 9. 
  Sometimes exclusivity is also required so that each digit is 
  restricted to appear exactly once.
  """
  
  * Wikipedia http://en.wikipedia.org/wiki/Pandigital_number
 
  * Also see my other pandigital number models:
    - MiniZinc: http://www.hakank.org/minizinc/pandigital_numbers.mzn
    - Gecode/R: http://www.hakank.org/gecode_r/pandigital_numbers.rb
    - Comet   : http://www.hakank.org/comet/pandigital_numbers.co
    - ECLiPSe : http://www.hakank.org/eclipse/pandigital_numbers.ecl
    - SICStus Prolog: http://www.hakank.org/sicstus/pandigital_numbers.pl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

//
// The options for this model.
//
class PandigitalOptions : public Options {

private:

  // Option-handling members
  Driver::UnsignedIntOption _base;         // -base: base to use
  Driver::UnsignedIntOption _start;        // -start: number to start (0 or 1)
  Driver::UnsignedIntOption _len1;         // -len1
  Driver::UnsignedIntOption _len2;         // -len2

  /*
  Driver::StringOption _int_var;           // -int-var: IntVarBranch
  Driver::StringOption _int_val;           // -int-val: IntValBranch
  */

public:

  // Initialize options for example with name 
  PandigitalOptions(const char* n, 
                    unsigned int len1, 
                    unsigned int len2, 
                    unsigned int base
                    )
    : Options(n),
      _base("-base",
                   "base to use",
                   base),
      _start("-start",
                "number to start (0 or 1).",
                1),
       _len1("-len1",
                    "length of first number ",
                    len1),
      _len2("-len2",
                   "length of second number ",
                   len2)  /*,
      _int_var("-int-var",
                      "options for IntVarBranch",
                      INT_VAR_MIN_MIN),
      _int_val("-int-val",
                      "options for IntValBranch",
                      INT_VAL_MIN)
      */

  {
    add(_base);
    add(_start);
    add(_len1);
    add(_len2);


    //
    // The names for these branching options are from the Gecode/FlatZinc mapping 
    // of the the MiniZinc branch options.
    //

    /*
    // IntVarBranch
    add(_int_var);
    _int_var.add(INT_VAR_NONE          , "input-order", "use VAR_NONE");
    _int_var.add(INT_VAR_SIZE_MIN      , "first-fail", "use VAR_SIZE_MIN");
    _int_var.add(INT_VAR_SIZE_MAX      , "anti-first-fail", "use VAR_SIZE_MAX");
    _int_var.add(INT_VAR_MIN_MIN       , "smallest", "use VAR_MIN_MIN");
    _int_var.add(INT_VAR_MAX_MAX       , "largest", "use VAR_MAX_MAX");
    _int_var.add(INT_VAR_DEGREE_MAX    , "occurrence", "use VAR_DEGREE_MAX");
    _int_var.add(INT_VAR_REGRET_MIN_MAX, "max-regret", "use VAR_REGRET_MIN_MAX");

    // IntValBranch
    add(_int_val);
    _int_val.add(INT_VAL_MIN           , "indomain-min", "use VAL_MIN");
    _int_val.add(INT_VAL_MAX           , "indomain-max", "use VAL_MAX");
    _int_val.add(INT_VAL_MED           , "indomain-median", "use VAL_MED");
    _int_val.add(INT_VAL_SPLIT_MIN     , "indomain-split", "use VAL_SPLIT_MIN");
    */

  }

  // Parse options from arguments argv (number is argc)
  void parse(int& argc, char* argv[]) {

    // Parse regular options
    Options::parse(argc,argv);

  }

  // Return base to use
  unsigned int base(void) const  { return _base.value();  }

  // Return the start number (0 or 1)
  unsigned int start(void) const { return _start.value(); }

  // Return len1
  unsigned int len1(void) const { return _len1.value(); }

  // Return len2
  unsigned int len2(void) const { return _len2.value(); }

  /*
  // Return IntVarBranch 
  unsigned int int_var(void) const { return _int_var.value(); }

  // Return IntValBranch 
  unsigned int int_val(void) const { return _int_val.value(); }
  */

};


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
void to_num(Space& space, IntVarArgs x, IntVar z, int base = 10, IntConLevel icl = ICL_BND) {
  
  int len = x.size();
  IntArgs coeffs(len);
  for(int r = 0; r < len; r++) {
    coeffs[r] = pow(base, len-r-1);
  }
  
  linear(space, coeffs, x, IRT_EQ, z, icl);
}



class Pandigital : public Script {
protected:

  int base;       // base to use
  int start;      // the digit to start with (0 or 1)
  int x_len;      // length of the x array
  int max_num;    // maximal number in domain
  int len1;       // length of first number
  int len2;       // length of second number
  IntVar num1;    // first number
  IntVar num2;    // second number
  IntVar res;     // res = num1 * num2
  IntVarArray x;  // array of the (distinct) digits 

public:

  Pandigital(const PandigitalOptions& opt) 
  : 
    base(opt.base()),
    start(opt.start()),
    x_len(base-start),
    max_num(pow(base,(base-3))),
    len1(opt.len1()),
    len2(opt.len2()),
    num1(*this, 1, max_num),
    num2(*this, 1, max_num),
    res(*this, 1, max_num),
    x(*this, x_len, start, base-1)
  {

    distinct(*this, x, opt.icl());

    // res == num 1 * num2
    mult(*this, num1, num2, res, opt.icl());
        

    // num1
    IntVarArgs n1(len1);
    int k = 0;
    for(int i = 0; i < len1; i++) {
      n1[k++] = x[i];
    }
    to_num(*this, n1, num1, base, opt.icl());
        
     
    // num2
    IntVarArgs n2(len2);
    k = 0;
    for(int i = len1; i < len1+len2; i++) {
      n2[k++] = x[i];
    }
    to_num(*this, n2, num2, base, opt.icl());
        
    // the product
    IntVarArgs prod(x_len-len1-len2);
    k = 0;
    for(int i = len1+len2; i < x_len; i++) {
      prod[k++] = x[i];
    }
    to_num(*this, prod, res, base, opt.icl());
    
    //
    // sanitizing and symmetry breaking
    //
    rel(*this, x[0] > 0, opt.icl());
    rel(*this, x[len1] > 0, opt.icl());
    rel(*this, x[len1+len2] > 0, opt.icl());
    
    rel(*this, num1 > 0, opt.icl());
    rel(*this, num1 < num2, opt.icl());
    rel(*this, num2 > 0, opt.icl());
    rel(*this, num2 < res, opt.icl());
    rel(*this, res > 0, opt.icl());
    
     
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_SPLIT_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << " base: " << base << std::endl;
    os << "num1: " << num1 << " num2: " << num2 << " res: " << res << std::endl;
    os << std::endl;
  }

  // Constructor for cloning s
  Pandigital(bool share, Pandigital& s) : Script(share,s),
                                          base(s.base) {
    x.update(*this, share, s.x);
    num1.update(*this, share, s.num1);
    num2.update(*this, share, s.num2);
    res.update(*this, share, s.res);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Pandigital(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  PandigitalOptions opt("Pandigital", 2, 2, 10);
  opt.parse(argc,argv);

  const unsigned int start = opt.start();
  const unsigned int base = opt.base();
  const unsigned int x_len = base-start;
  std::cout << "base: " << base << std::endl;
  std::cout << "start: " << start << std::endl;
  std::cout << "x_len: " << x_len << std::endl;

  //
  // Loop through combinations of len1 and len2
  //
  for (unsigned int len1 = 1; len1 < 2+(x_len / 3); len1++) {
    for(unsigned int len2 = 1; len2 < 2+(x_len / 3); len2++) {
      if (len1 <= len2) {
        PandigitalOptions opt("Pandigital", len1, len2, base);
        opt.solutions(0);
        opt.parse(argc,argv);
        Script::run<Pandigital,DFS,PandigitalOptions>(opt);
      }
    }
  }

  return 0;
}


