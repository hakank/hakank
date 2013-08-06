/*

  Scheduling speakers in Gecode.

  From Rina Dechter, Constraint Processing, page 72
  Scheduling of 6 speakers in 6 slots.

  Also, compare with the following models:
  - MiniZinc: http://www.hakank.org/minizinc/scheduling_speakers.mzn
  - SICStus Prolog: http://www.hakank.org/sicstus/scheduling_speakers.pl
  - ECLiPSe: http://www.hakank.org/eclipse/scheduling_speakers.ecl


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


class Speakers : public Script {
protected:

  static const int n = 6; // number of speakers
  IntVarArray x;          // speakers


public:

  // Actual model
  Speakers(const SizeOptions& opt) : 
    x(*this, n, 1, n)
  {

    //
    // available slots:
    //

    /*
    // First solution: populate from a integer array 
    // where the first number represents the number of items
    int __available[] = {
    // size: available  // Reasoning. Step# : reason
      4,  3,4,5,6,      // #2: the only one with 6 after speaker F -> 1
      2,  3,4,          // #5: 3 or 4
      4,  2,3,4,5,      // #3: only with 5 after F -> 1 and A -> 6
      3,  2,3,4,        // #4: only with 2 after C -> 5 and F -> 1 
      2,  3,4,          // #5: 3 or 4
      6,  1,2,3,4,5,6   // #1: the only with 1
    };

    // convert to IntSet
    IntSet _available[n];
    int s = 0;
    for(int i = 0; i < n; i++) {
      int num = __available[s++];
      IntArgs tmp;
      for(int j = 0; j < num; j++) {
        tmp << __available[s++];
      }
      _available[i] = IntSet(tmp);
    }
    // and then to IntSetArgs
    IntSetArgs available(n, _available);
    */

    //
    // Oh, this is much easier, at least for this small example
    //
    IntSetArgs available(n);
    available[0] = IntSet( IntArgs() << 3 << 4 << 5 << 6 );
    available[1] = IntSet( IntArgs() << 3 << 4 );
    available[2] = IntSet( IntArgs() << 2 << 3 << 4 << 5 );
    available[3] = IntSet( IntArgs() << 2 << 3 << 4 );
    available[4] = IntSet( IntArgs() << 3 << 4 );
    available[5] = IntSet( IntArgs() << 1 << 2 << 3 << 4 << 5 << 6 );

    distinct(*this, x, opt.icl());

    for(int i = 0; i < n; i++) {
      // From Modeling and Programming in Gecode, 
      // section 5.2.2, Relation constraints:
      // """
      // If x is a set variable and y is an integer variable, then
      //     rel(home, x, SRT_SUP, y);
      // constrains x to be a superset of the singleton set {y}, 
      // which means that y must be an element of x.
      // """
      // 
      // corresponds to: forall(i in 1..n) (x[i] in available[i])
      //
      // rel(*this, SetVar(*this, available[i],available[i]), SRT_SUP, x[i]);
      // this is easier:
      rel(*this, singleton(x[i]) <= available[i]);
    }

    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    
  }

  // Constructor for cloning s
  Speakers(bool share, Speakers& s) : Script(share,s) {
    x.update(*this, share, s.x);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Speakers(share,*this);
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x: " << x << std::endl;
    os << std::endl;
  }

};

/** 
 *  main
 */
int
main(int argc, char* argv[]) {
  SizeOptions opt("Speakers");
  opt.solutions(0);
  opt.parse(argc,argv);

  Script::run<Speakers,DFS,SizeOptions>(opt);

  return 0;
}


