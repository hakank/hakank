/*
  SEND + MORE = MONEY in any base in Gecode.

  This model solves the standard constraint programming problem
     SEND
  +  MORE
  = MONEY

  where the digits must be distinct and can be in any (but same) base.

  The problem was described in the blog post (last section):
  "Some other Gecode/R models, mostly recreational mathematics"
  http://www.hakank.org/constraint_programming_blog/2009/01/some_other_gecoder_models_most_1.html

  Also, see the following models:
  MiniZinc: http://www.hakank.org/minizinc/send_more_money_any_base.mzn
  Gecode/R: http://www.hakank.org/gecode_r/send_more_money_any_base.rb
  Comet   : http://www.hakank.org/comet/send_more_money_any_base.co


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/


 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

//
// This model was directly derived from Gecode's example money.cpp
//
class SendMoreMoney : public Script {
protected:

  // base to use (argument of the program)
  const int base; 

  // Number of letters
  static const int num_letters = 8;

  // Array of letters
  IntVarArray x;

public:

  // Model variants
  enum {
    MODEL_SINGLE, //< Use single linear equation
    MODEL_CARRY   //< Use carries
  };

  // Actual model
  SendMoreMoney(const SizeOptions& opt) : 
    base(opt.size()),
    x(*this,num_letters, 0, base-1)
     {

    std::cout << "Using base: " << base << std::endl;

    IntVar
      s(x[0]), e(x[1]), n(x[2]), d(x[3]),
      m(x[4]), o(x[5]), r(x[6]), y(x[7]);

    rel(*this, s, IRT_NQ, 0);
    rel(*this, m, IRT_NQ, 0);

    distinct(*this, x, opt.icl());

    int ten          = base;
    int hundred      = base*ten;
    int thousand     = base*hundred;
    int ten_thousand = base*thousand;

    switch (opt.model()) {
    case MODEL_SINGLE:
      rel(*this, 
                                 thousand*s + hundred*e + ten*n + d
              +                  thousand*m + hundred*o + ten*r + e
             == ten_thousand*m + thousand*o + hundred*n + ten*e + y,
           opt.icl());
      break;

    case MODEL_CARRY:
      {
        IntVar c0(*this,0,1), c1(*this,0,1), c2(*this,0,1), c3(*this,0,1);
        rel(*this,    d+e == y+base*c0, opt.icl());
        rel(*this, c0+n+r == e+base*c1, opt.icl());
        rel(*this, c1+e+o == n+base*c2, opt.icl());
        rel(*this, c2+s+m == o+base*c3, opt.icl());
        rel(*this, c3     == m,       opt.icl());
      }
      break;
    default: GECODE_NEVER;
    }

    branch(*this, x, INT_VAR_NONE(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "\t" << x << std::endl;
  }

  // Constructor for cloning
  SendMoreMoney(bool share, SendMoreMoney& s) : Script(share,s), base(s.base) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new SendMoreMoney(share,*this);
  }
};

int 
main(int argc, char* argv[]) {
  SizeOptions opt("SEND+MORE=MONEY any base");
  opt.model(SendMoreMoney::MODEL_SINGLE);
  opt.model(SendMoreMoney::MODEL_SINGLE, "single", "use single linear equation");
  opt.model(SendMoreMoney::MODEL_CARRY, "carry", "use carry");
  opt.icl(ICL_DOM);
  opt.solutions(0);
  opt.iterations(20000);
  opt.parse(argc,argv);
  // set default base = 10
  if (!opt.size()) {
    opt.size(10);
  }
  Script::run<SendMoreMoney,DFS,SizeOptions>(opt);
  return 0;
}

// STATISTICS: example-any

