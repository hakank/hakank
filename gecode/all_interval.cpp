/*
  
  All interval problem  in Gecode.

   CSPLib problem number 7
  http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob007/index.html
  """
  Given the twelve standard pitch-classes (c, c , d, ...), represented by 
  numbers 0,1,...,11, find a series in which each pitch-class occurs exactly 
  once and in which the musical intervals between neighbouring notes cover 
  the full set of intervals from the minor second (1 semitone) to the major 
  seventh (11 semitones). That is, for each of the intervals, there is a 
  pair of neigbhouring pitch-classes in the series, between which this 
  interval appears. The problem of finding such a series can be easily 
  formulated as an instance of a more general arithmetic problem on Z_n, 
  the set of integer residues modulo n. Given n in N, find a vector 
  s = (s_1, ..., s_n), such that (i) s is a permutation of 
  Z_n = {0,1,...,n-1}; and (ii) the interval vector 
  v = (|s_2-s_1|, |s_3-s_2|, ... |s_n-s_{n-1}|) is a permutation of 
  Z_n-{0} = {1,2,...,n-1}. A vector v satisfying these conditions is 
  called an all-interval series of size n; the problem of finding such 
  a series is the all-interval series problem of size n. We may also be 
  interested in finding all possible series of a given size. 
  """
 
  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/all_interval.mzn
  * Comet   : http://www.hakank.org/comet/all_interval.co 
  * Gecode/R: http://www.hakank.org/gecode_r/all_interval.rb
  * ECLiPSe: http://www.hakank.org/eclipse/all_interval.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/all_interval.pl


  Note: This is a fairly true port from the MiniZinc model.
  A faster implementation is in the Gecode distribution:
  http://www.gecode.org/doc-latest/reference/all-interval_8cpp_source.html
  

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


class AllInterval : public Script {
protected:

  int n;

  IntVarArray x;
  IntVarArray diffs;

public:

  AllInterval(const SizeOptions& opt) 
    : 
    n(opt.size()),
    x(*this, n, 1, n),
    diffs(*this, n-1, 0, n-1)
  {

    cout << "Size: " << n << endl;

    distinct(*this, x, opt.icl());
    distinct(*this, diffs, opt.icl());

    for(int k = 0; k < n-1; k++) {
      rel(*this, diffs[k] == abs(x[k+1] - x[k]), opt.icl());
    }

    // symmetry breaking
    rel(*this, x[0] < x[n-1], opt.icl());
    rel(*this, diffs[0] < diffs[1], opt.icl());

    // branching
    branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "x    : " << x << endl;
    os << "diffs: " << diffs << endl;
    os << endl;
  }


  // Constructor for cloning s
  AllInterval(bool share, AllInterval& s) : Script(share,s) {
    x.update(*this, share, s.x);
    diffs.update(*this, share, s.diffs);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new AllInterval(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  SizeOptions opt("AllInterval");

  opt.icl(ICL_DOM);
  opt.solutions(0);
  opt.size(12);

  opt.parse(argc,argv);

  Script::run<AllInterval,DFS,SizeOptions>(opt);
    
  return 0;
}


