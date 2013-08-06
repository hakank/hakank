/*

  Devil's word in Gecode.

 Translate each character in a word to ASCII value and then try
  to sum its values (either positive or negative) to a total.
  
  E.g. "hakankkjellerstrand" and total 666 gives 359 solutions.
  Here is the first:
  +104 +97 +107 +97 +110 +107 +106 -101 +108 +108 -101 +114 +115 +116 -114 -97 -110 -100
 
  Also see 
   * my CGI program "Devil's Word"
     http://www.hakank.org/data_snooping/666.cgi
   * MiniZinc: http://www.hakank.org/minizinc/devils_words.mzn
   * Gecode/R: http://www.hakank.org/gecoder/devils_word.rb
   * ECLiPSe: http://www.hakank.org/eclipse/devils_word.ecl
   * SICStus Prolog: http://www.hakank.org/sicstus/devils_word.pl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


class DevilsWord : public Script {
protected:

  static const int n = 19;
  static const int total = 666;

  IntVarArray plus;
  IntVarArray minus;
  IntVarArray result;

public:

  DevilsWord(const Options& opt) 
  : 
    plus(*this, n, 0, 1),
    minus(*this, n, 0, 1),
    result(*this, n, -255, 255)
  {

    // My name ("Håkan Kjellerstrand") as ASCII numbers.
    // Cf http://www.hakank.org/data_snooping/666.cgi?name=H%E5kan+Kjellerstrand&submit=ok
    // which gives the solution:
    // +72+229+107+97+110+32+75-106+101+108-108+101-114-115-116-114+97+110+100 = 666
    int arr[] = {
      72, 229, 107, 97, 110, 32, 75, 106, 101, 108, 108, 101, 114, 
      115, 116, 114, 97, 110, 100
    };


    for(int i = 0; i < n; i++) {
      // just one plus or minus
      rel(*this, plus[i] + minus[i] == 1);
      // the result of +/-
      rel(*this, result[i] == arr[i]*plus[i] + (-arr[i]*minus[i]));
    }
    rel(*this, total == sum(result));

    branch(*this, result, INT_VAR_SIZE_MAX(), INT_VAL_MIN()); 
    branch(*this, plus, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 
    branch(*this, minus, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "total : " << total << std::endl;
    os << "plus  : " << plus << std::endl;
    os << "minus : " << minus << std::endl;
    os << "result: " << result << std::endl;
    os << std::endl;
  }

  // Constructor for cloning s
  DevilsWord(bool share, DevilsWord& s) : Script(share,s) {
    plus.update(*this, share, s.plus);
    minus.update(*this, share, s.minus);
    result.update(*this, share, s.result);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new DevilsWord(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("DevilsWord");

  opt.solutions(0);
  opt.parse(argc,argv);

  Script::run<DevilsWord,DFS,Options>(opt);

  return 0;
}


