/*
  
  Miss Manners' seating problem in Gecode.

  
  From http://4c110.ucc.ie/cpstandards/index.php/en/standards/java/examples/27
  """
  The "Miss Manners" problem is a notorious benchmark for rule engines. 
  The problem is to find an acceptable seating arrangement for guests at 
  a dinner party.  It should match people with the same hobbies (at 
  least one), and to seat everyone next to a member of the opposite sex. 
  """

  The data is presented in the Excel file: 
  http://4c110.ucc.ie/cpstandards/files/Manners.xls
  
  The MiniZinc data filers:
   http://www.hakank.org/minizinc/miss_manners_16.dzn
   http://www.hakank.org/minizinc/miss_manners_64.dzn
   http://www.hakank.org/minizinc/miss_manners_128.dzn

  Also, see 
   - http://docs.codehaus.org/display/DROOLS/Miss+Manners+Example
   - http://blog.athico.com/2009/05/miss-manners-2009-yet-another-drools.html
   - http://it.toolbox.com/blogs/thinking-out-loud/industry-analysts-and-rules-engines-2349
     Refers to OPS5 benchmark suite: 
     ftp://ftp.cs.utexas.edu/pub/ops5-benchmark-suite/


  Compare with the following models
  * MiniZinc: http://www.hakank.org/minizinc/miss_manners.mzn
      Data for the MiniZinc version:
    - http://www.hakank.org/minizinc/miss_manners_16.dzn
    - http://www.hakank.org/minizinc/miss_manners_64.dzn
    - http://www.hakank.org/minizinc/miss_manners_128.dzn
  * SICStus Prolog: http://www.hakank.org/sicstus/miss_manners.pl
  * ECLiPSe: http://www.hakank.org/eclipse/miss_manners.ecl
  
  Note: I assume a circular table placement here.

  Also, a more interesting (and demanding) problem should be to maximize 
  the number of common hobbies of the neighbours.
  (On the other hand, not sharing all hobbies with one's neighbours could 
  give raise to new interests an more animated discussions.)


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/set.hh>
#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using std::cout;
using std::endl;

using namespace Gecode;


//
// Alternating male and females
//
void alternate_MF(Space &space, IntVarArray seating, IntArgs gender, int i, int j, IntConLevel icl = ICL_DOM) {

  int n = seating.size();
  IntVar s1(space, 0,n-1);
  IntVar s2(space, 0,n-1);
  element(space, gender, seating[i], s1, icl);
  element(space, gender, seating[j], s2, icl);
  rel(space, s1 != s2);

}


//
// Number of common hobbies between two neighbours
//
void get_common_hobbies(Space &space, IntVarArray seating, IntSetArgs hobbies, int num_hobbies, int i, int j, IntVar card, IntConLevel icl = ICL_DOM) {

  SetVar s1(space, IntSet::empty, IntSet(1,num_hobbies));
  SetVar s2(space, IntSet::empty, IntSet(1,num_hobbies));  
  element(space, hobbies, seating[i], s1);
  element(space, hobbies, seating[j], s2);

  // intersection of the hobbies
  SetVar u(space, IntSet::empty, IntSet(1,num_hobbies));
  rel(space, s1, SOT_INTER, s2, SRT_EQ, u);
  // number of common interests
  cardinality(space, u, card);

}



class MissManners : public MaximizeScript {
protected:

  const static int n = 16;          // number of persons at the party
  const static int num_hobbies = 3; // number of hobbies

  // decision variables
  IntVarArray seating;    // the seating (positions)
  IntVar common_hobbies;  // number of common hobbies, to be maximized
  
  // IntVarArray cards; // number of common hobbies
            
public:

  // search engines
  enum {
    SEARCH_DFS,
    SEARCH_BAB,
  };

  MissManners(const SizeOptions& opt) 
    : 
    seating(*this, n, 0, n-1),
    common_hobbies(*this, 0, (n+1)*num_hobbies)
  {

    enum {M, F};
    int _gender[] = {M,F,M,M,M,M,F,M,M,M,F,F,F,F,F,F};
    IntArgs gender(n, _gender);

    int num_hobbies = 3;

    // The hobbies for each person 
    // (here I don't care about the order).
    // First is the number of hobbies, then the hobbies.
    // There is probably/hopefully a better way of doing 
    // this in C++.
    int _hobbies[] = {
      3, 2,1,3,
      3, 2,1,3,
      2, 2,3,
      3, 3,2,1,
      3, 2,1,3,
      3, 2,3,1,
      3, 1,2,3,
      2, 3,1,
      3, 2,3,1,
      3, 3,2,1,
      3, 1,3,2,
      3, 3,1,2,
      2, 2,3,
      2, 1,2,
      3, 2,3,1,
      2, 2,3
    };
    
    // convert to IntSet
    IntSet __hobbies[n];
    int s = 0;
    for(int i = 0; i < n; i++) {
      int num = _hobbies[s++];
      IntArgs tmp;
      for(int j = 0; j < num; j++) {
        tmp << _hobbies[s++];
      }
      __hobbies[i] = IntSet(tmp);
    }
    // and then to IntSetArgs
    IntSetArgs hobbies(n, __hobbies);


    // unique positions
    distinct(*this, seating, opt.icl());

    // alternate between M and F
    for(int i = 1; i < n; i++) {
      alternate_MF(*this, seating, gender, i, i-1, opt.icl());
    }
    // close the circle
    alternate_MF(*this, seating, gender, 0, n-1, opt.icl());

    // total number of common hobbies between to neighbours 
    // (to maximize)
    IntVarArgs cards(*this, n, 0, num_hobbies);
    for(int i = 1; i < n; i++) {
      get_common_hobbies(*this, seating, hobbies, num_hobbies, i, i-1, cards[i], opt.icl());
    }
    // close the circle
    get_common_hobbies(*this, seating, hobbies, num_hobbies, 0, n-1, cards[0], opt.icl());


    // total number of common hobbies
    linear(*this, cards, IRT_EQ, common_hobbies);

    // symmetry breaking
    // rel(*this, seating[0] == 0);


    // branching
    // branch(*this, seating, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

    branch(*this, seating, INT_VAR_DEGREE_MAX(), INT_VAL_MAX());

    // branch(*this, common_hobbies, INT_VAL_MAX());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "seating: " << seating << endl;
    os << "#common_hobbies: " << common_hobbies << endl;
    os << endl;
  }

  // Return cost
  virtual IntVar cost(void) const {
    return common_hobbies;
  }


  // Constructor for cloning s
  MissManners(bool share, MissManners& s) : MaximizeScript(share,s) {
    seating.update(*this, share, s.seating);
    common_hobbies.update(*this, share, s.common_hobbies);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new MissManners(share,*this);
  }

};


int
main(int argc, char* argv[]) {

  SizeOptions opt("MissManners");

  opt.solutions(0);
  opt.icl(ICL_DOM);
  opt.search(MissManners::SEARCH_BAB);
  opt.search(MissManners::SEARCH_DFS, "dfs");
  opt.search(MissManners::SEARCH_BAB, "bab");


  opt.parse(argc,argv);
  if (opt.size()) {
    opt.search(MissManners::SEARCH_DFS);
  }

  if (opt.search() == MissManners::SEARCH_DFS) {
    Script::run<MissManners,DFS,SizeOptions>(opt);
  } else {
    MinimizeScript::run<MissManners,BAB,SizeOptions>(opt);
  }

  return 0;
}


