/*
  
  Recreational mathematics in Gecode.

    Problem 1.4 from 
  Averbach & Chein "Problem Solving Through Recreational Mathematics".
  """
  Messr Baker, Dyer, Farmer, Glover, and Hosier are seated around a circular table,
  playing poker. Each gentleman is the namesake of the profession of one of 
  the others.

  The dyer is seated two places to the left of Mr Hosier.
  The baker sits two places to Mr Baker's right.
  The farmer is seated to the left of Mr Farmer.
  Mr Dyer is on the glover's right.

  What is the name of the dyer?
  """

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

using std::cout;
using std::endl;


class Averbach : public Script {
protected:

  const static int n = 5;
  IntVarArray names;
  IntVarArray profession;

  IntVar dyer;
  IntVar the_dyer;

public:

  // search engines
  enum {
    SEARCH_DFS,
  };

  Averbach(const Options& opt) 
    : 
    names(*this, n, 0, n-1),
    profession(*this, n, 0, n-1),
    dyer(*this, 0, n-1)
  {

    // enum {baker, dyer, farmer, glover, hosier};
    // enum {Baker, Dyer, Farmer, Glover, Hosier};

    IntVar 
      baker(profession[0]),
      // dyer(profession[1]), 
      farmer(profession[2]), 
      glover(profession[3]), 
      hosier(profession[4]);

    IntVar 
      Baker(names[0]),
      Dyer(names[1]), 
      Farmer(names[2]), 
      Glover(names[3]), 
      Hosier(names[4]);

    rel(*this, dyer == profession[1]);

    distinct(*this, names);
    distinct(*this, profession);


    //  Symmetry breaking
    rel(*this, Baker == 1);

    // Each gentleman is the namesake of the 
    // profession of one of the others
    /*
    rel(*this, baker  != Baker);
    rel(*this, dyer   != Dyer);
    rel(*this, farmer != Farmer);
    rel(*this, glover != Glover);
    rel(*this, hosier != Hosier);
    */

    // or simpler:
    for(int i = 0; i < n; i++) {
      rel(*this, names[i] != profession[i]);
    }

    // IntVar c5(*this, 5, 5);
    // The dyer is seated two places to the left of Mr Hosier.
    // alternative, explicit version:
    // mod(*this, expr(*this,Hosier - 2), c5, dyer);
    rel(*this, dyer == (Hosier - 2) % 5);

    // The baker sits two places to Mr Baker's right.
    // mod(*this, expr(*this, Baker + 2),  c5, baker);
    rel(*this, baker == (Baker + 2) % 5);

    // The farmer is seated to the left of Mr Farmer.
    // mod(*this, expr(*this,Farmer - 1), c5, farmer);
    rel(*this, farmer == (Farmer - 1) % 5);

    // Mr Dyer is on the glover's right.
    // mod(*this, expr(*this,glover + 1), c5, Dyer);
    rel(*this, Dyer == (glover + 1) % 5);

    // branching
    branch(*this, names, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    // branch(*this, profession, INT_VAR_SIZE_MAX(), INT_VAL_MAX());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "names: " << names << endl;
    os << "profession: " << profession << endl;
    os << "dyer: " << dyer << endl;
    std::string _names[] = {"Baker", "Dyer", "Farmer", "Glover", "Hosier"};
    os << "The name of the dyer (" << dyer << ") is " << _names[dyer.val()] << endl;
  }


  // Constructor for cloning s
  Averbach(bool share, Averbach& s) : Script(share,s) {
    names.update(*this, share, s.names);
    profession.update(*this, share, s.profession);
    dyer.update(*this, share, s.dyer);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Averbach(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("Averbach");

  opt.solutions(0);
  opt.icl(ICL_BND);
  opt.search(Averbach::SEARCH_DFS);
  opt.search(Averbach::SEARCH_DFS, "dfs");



  opt.parse(argc,argv);

  if (opt.search() == Averbach::SEARCH_DFS) {
    Script::run<Averbach,DFS,Options>(opt);
  } 

  return 0;
}


