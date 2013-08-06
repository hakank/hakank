/*
  
  Hamming distance in Gecode.

  I.e. the number of bits differing in two (binary) arrays.
  See http://en.wikipedia.org/wiki/Hamming_distance
 
  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/hamming_distance.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/hamming_distance.pl
  * ECLiPSe: http://www.hakank.org/eclipse/hamming_distance.ecl

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

void hamming_distance(Space& space, 
                      IntVarArray a, 
                      IntVarArray b, 
                      int n, 
                      IntVar diffs) {

  BoolVarArgs d;
  for(int i = 0; i < n; i++) {
    d << expr(space, a[i] != b[i]);
  }
  rel(space, diffs == sum(d));
}

class HammingDistance : public Script {
protected:

  static const int n = 6; // length of the arrays

  IntVarArray a;
  IntVarArray b;

  IntVar diffs; // number of differences

public:

  HammingDistance(const Options& opt) 
    : 
    a(*this, n, 0, 1),
    b(*this, n, 0, 1),
    diffs(*this, 0, n)
  {

    int _atmp[] = {1,1,1,1,1,1};
    IntArgs atmp(n, _atmp);
    for(int i = 0; i < n; i++) {
      rel(*this, a[i] == atmp[i]);
    }

    hamming_distance(*this, a, b, n, diffs);

    // exactly 2 differences
    rel(*this, diffs == 2);

    // branching
    branch(*this, a, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    branch(*this, b, INT_VAR_SIZE_MIN(), INT_VAL_MIN());


  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "a: " << a << endl;
    os << "b: " << b << endl;
    os << "diffs: " << diffs << endl;
    os << endl;

  }


  // Constructor for cloning s
  HammingDistance(bool share, HammingDistance& s) : Script(share,s) {
    a.update(*this, share, s.a);
    b.update(*this, share, s.b);
    diffs.update(*this, share, s.diffs);

  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new HammingDistance(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("HammingDistance");

  opt.solutions(0);
  opt.icl(ICL_BND);

  opt.parse(argc,argv);

  Script::run<HammingDistance,DFS,Options>(opt);
    
  return 0;
}


