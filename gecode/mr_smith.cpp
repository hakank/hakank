/*
  
  Mr Smith problem in Gecode.

   From an IF Prolog example (http://www.ifcomputer.de/)
   """
   The Smith family and their three children want to pay a visit but they
   do not all have the time to do so. Following are few hints who will go
   and who will not:
       o If Mr Smith comes, his wife will come too.
       o At least one of their two sons Matt and John will come.
       o Either Mrs Smith or Tim will come, but not both.
       o Either Tim and John will come, or neither will come.
       o If Matt comes, then John and his father will
         also come.
    """

   The answer should be:
    Mr_Smith_comes      =  0
    Mrs_Smith_comes     =  0
    Matt_comes          =  0
    John_comes          =  1
    Tim_comes           =  1


  Compare with the following models:
  * SICStus Prolog: http://www.hakank.org/sicstus/mr_smith.pl
  * ECLiPSe: http://www.hakank.org/eclipse/mr_smith.ecl

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


class MrSmith : public Script {
protected:

  const static int n = 5;

  BoolVarArray L;

public:

  MrSmith(const Options& opt) 
    : 
    L(*this, n, 0, 1)
  {

    BoolVar 
      Mr_Smith(L[0]), 
      Mrs_Smith(L[1]), 
      Matt(L[2]), 
      John(L[3]), 
      Tim(L[4]);

    // If Mr Smith comes, his wife will come too.
    rel(*this, Mr_Smith >> Mrs_Smith);

    // At least one of their two sons Matt and John will come.
    rel(*this, Matt || John);

    // Either Mrs Smith or Tim will come, but not both.
    rel(*this, Mrs_Smith + Tim == 1);
    // xor(Mrs_Smith, Tim),

    // Either Tim and John will come, or neither will come.
    rel(*this, Tim == John);

    // If Matt comes, then John and his father will also come.
    rel(*this, Matt >> (John && Mr_Smith));


    // branching
    branch(*this, L, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "L: " << L << endl;
  }


  // Constructor for cloning s
  MrSmith(bool share, MrSmith& s) : Script(share,s) {
    L.update(*this, share, s.L);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new MrSmith(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("MrSmith");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<MrSmith,DFS,Options>(opt);
    
  return 0;
}


