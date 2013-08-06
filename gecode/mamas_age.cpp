/*
  
  Mama's age problem in Gecode.

   Mamma's Age from "Amusements in Mathematics, Dudeney", number 40.
   """
   Tommy: "How old are you, mamma?"
   Mamma: "Our three ages add up to exactly seventy years."
   Tommy: "And how old are you, papa?"
   Papa: "Just six times as old as you, my son."
   Tommy: "Shall I ever be half as old as you, papa?"
   Papa: "Yes, Tommy; and when that happens our three ages will add up to
   exactly twice as much as today."

   Can you find the age of Mamma?
   """

  Compare with the following models:
  * SICStus Prolog: http://www.hakank.org/sicstus/mamas_age.pl
  * ECLiPSe: http://www.hakank.org/eclipse/mamas_age.ecl

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


class MamasAge : public Script {
protected:

  const static int n = 4;

  IntVarArray LD;

public:

  MamasAge(const Options& opt) 
    : 
    LD(*this, n, 0, 500)
  {

    IntVar 
      M(LD[0]), 
      P(LD[1]), 
      T(LD[2]),
      I(LD[3]);
     
    // multiply with 12 (months)
    rel(*this,
        M + P + T == 70 * 12 &&
        6 * T == P &&
        (T + I) * 2 == P + I &&
        M + I + P + I + T + I == 2 * (M + P + T)
        );

    // branching
    branch(*this, LD, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "LD: " << LD << endl;
    os << "Mama: " << LD[0].val() / 12 << " year" << endl;
    os << "Papa: " << LD[1].val() / 12 << " year" << endl;
    os << "Tommy: " << LD[2].val() / 12 << " year" << endl;
  }


  // Constructor for cloning s
  MamasAge(bool share, MamasAge& s) : Script(share,s) {
    LD.update(*this, share, s.LD);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new MamasAge(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("MamasAge");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<MamasAge,DFS,Options>(opt);
    
  return 0;
}


