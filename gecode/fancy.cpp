/*

  Mr Greenguest puzzle (fancy dress) in Gecode.
 
  Problem (and LPL) code in
 
  http://diuflx71.unifr.ch/lpl/GetModel?name=/demo/demo2
 
  """
  (** Mr. Greenfan wants to give a dress party where the male guests
   * must wear green dresses. The following rules are given:
   * 1 If someone wears a green tie he has to wear a green shirt.
   * 2 A guest may only wear green socks and a green shirt 
   *   if he wears a green tie or a green hat.
   * 3 A guest wearing a green shirt or a green hat or who does
   *   not wear green socks must wear a green tie.
   * 4 A guest who is not dressed according to rules 1-3 must
   *   pay a $11 entrance fee.
   * Mr Greenguest wants to participate but owns only a green shirt 
   * (otherwise he would have to pay one for $9). He could buy 
   * a green tie for $10, a green hat (used) for $2 and green socks
   * for $12.
   * What is the cheapest solution for Mr Greenguest to participate?
   *)
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minzinc/fancy.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/fancy.pl
  * ECLiPSe: http://www.hakank.org/eclipse/fancy.ecl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/


#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/set.hh>

using namespace Gecode;

using std::cout;
using std::endl;

class FancyDress : public MinimizeScript {
protected:

  BoolVar t;
  BoolVar h;
  BoolVar r;
  BoolVar s;
  BoolVar n;
  IntVar total_cost;

public:

  // Search variants
  enum {
    SEARCH_DFS,     // Use depth first search
    SEARCH_BAB,     // Use branch and bound to optimize
  };


  FancyDress(const Options& opt) 
    : 
    t(*this, 0, 1),
    h(*this, 0, 1),
    r(*this, 0, 1),
    s(*this, 0, 1),
    n(*this, 0, 1),
    total_cost(*this, 0,100)

  {

    // This is a straight translation from the LPL code
    rel(*this, 
        ( (t >> r) || n) &&
        ( ((s || r) >> (t || h)) || n )  &&
        ( ((r || h || !s) >> t) || n )
        );

    rel(*this, total_cost == 10*t + 2*h + 12*s + 11*n);

    // branching
    branch(*this, t, INT_VAL_MIN());
    branch(*this, h, INT_VAL_MIN());
    branch(*this, r, INT_VAL_MIN());
    branch(*this, s, INT_VAL_MIN());
    branch(*this, n, INT_VAL_MIN());
    branch(*this, total_cost, INT_VAL_MIN());
 
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {

    os << "t: " << t << endl;
    os << "h: " << h << endl;
    os << "r: " << r << endl;
    os << "s: " << s << endl;
    os << "n: " << n << endl;
    os << "total_cost: " << total_cost << endl;
    os << endl;
  }


  // Constructor for cloning s
  FancyDress(bool share, FancyDress& ss) : MinimizeScript(share,ss) {
    t.update(*this, share, ss.t);
    h.update(*this, share, ss.h);
    r.update(*this, share, ss.r);
    s.update(*this, share, ss.s);
    n.update(*this, share, ss.n);
    total_cost.update(*this, share, ss.total_cost);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new FancyDress(share,*this);
  }

  virtual IntVar cost(void) const {
    return total_cost;
  }

};


int
main(int argc, char* argv[]) {

  Options opt("FancyDress");

  opt.search(FancyDress::SEARCH_BAB);
  opt.search(FancyDress::SEARCH_DFS, "dfs");
  opt.search(FancyDress::SEARCH_BAB, "bab");

  opt.solutions(0);
  opt.parse(argc,argv);

  switch (opt.search()) {
    case FancyDress::SEARCH_DFS:
      MinimizeScript::run<FancyDress,DFS,Options>(opt); break;
    case FancyDress::SEARCH_BAB:
      MinimizeScript::run<FancyDress,BAB,Options>(opt); break;
    }




  return 0;
}
