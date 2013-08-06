/*

  Global constraint distribute in Gecode.

  Decomposition of global constraint distribute.

  From MiniZinc globals.mzn:
  """
  Requires that 'card[i]' is the number of occurences of 'value[i]' in 'base'.
  XXX: currently the values in 'value' need not be distinct.  Perhaps they
  should be?
  """

  Compare with the following models:
  * Comet   : http://www.hakank.org/comet/distribute.co
  * ECLiPSe: http://www.hakank.org/eclipse/distribute.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/distribute.pl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


void distribute(Space& space, IntVarArray dcard, IntVarArray value, IntVarArray base) {

  for(int i = 0; i < dcard.size(); i++) {
    BoolVarArgs tmp;
    for(int j = 0; j < base.size(); j++) {
      tmp << expr(space, value[i] == base[j]);
    }
    rel(space, dcard[i] == sum(tmp));
  }

}

// copy all values except 0 from an array to an IntVarArray
void copy_except_0(Space& space, IntVarArray x, int _x[]) {

  for(int i = 0; i < x.size(); i++) {
    if (_x[i] != 0) {
      rel(space, x[i] == _x[i]);
    }
  }
}

class Distribute : public Script {
protected:

  static const int n = 4;
  static const int m = 7;

  IntVarArray dcard;
  IntVarArray value;
  IntVarArray base;

public:

  Distribute(const Options& opt) 
  : 
    dcard(*this, n, 1, 10),
    value(*this, n, 1, 10),
    base(*this, m, 1,10)
  {

    // 0 is an unconstrained value
    int _dcard[] = {4, 0, 1, 0};
    copy_except_0(*this, dcard, _dcard);

    int _value[] = {0, 7, 8, 9};
    copy_except_0(*this, value, _value);

    int _base[] = {0, 7, 6, 8, 6, 9, 0};
    copy_except_0(*this, base, _base);

    distribute(*this, dcard, value, base);


    branch(*this, dcard, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 
    branch(*this, value, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 
    branch(*this, base, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "dcard: " << dcard << std::endl;
    os << "value: " << value << std::endl;
    os << "base : " << base << std::endl;
    os << std::endl;
  }

  // Constructor for cloning s
  Distribute(bool share, Distribute& s) : Script(share,s) {
    dcard.update(*this, share, s.dcard);
    value.update(*this, share, s.value);
    base.update(*this, share, s.base);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Distribute(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("Distribute");

  opt.solutions(0);
  opt.parse(argc,argv);

  Script::run<Distribute,DFS,Options>(opt);

  return 0;
}


