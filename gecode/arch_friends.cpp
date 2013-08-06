/*
  
  Arch Friends puzzle (Delle Logic Puzzles) in Gecode.

  From http://brownbuffalo.sourceforge.net/ArchFriendsClues.html
  """
  Title: Arch Friends
  Author: Mark T. Zegarelli
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 7
  Stars: 1

  Harriet, upon returning from the mall, is happily describing her four
  shoe purchases to her friend Aurora. Aurora just loves the four
  different kinds of shoes that Harriet bought 
     (ecru espadrilles,fuchsia flats, purple pumps, and suede sandals),
  but Harriet can't recall at which different store 
     (Foot Farm, Heels in a Handcart, The Shoe Palace, or Tootsies) 
  she got each pair. Can you help these two figure out the order in
  which Harriet bought each pair of shoes, and where she bought each?

  1. Harriet bought fuchsia flats at Heels in a Handcart.
  2. The store she visited just after buying her purple pumps was not
     Tootsies.
  3. The Foot Farm was Harriet's second stop.
  4. Two stops after leaving The Shoe Place, Harriet bought her suede
     sandals.

Determine: Order - Shoes - Store 
  """


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/arch_friends.mzn
  * SICStus Prolog   : http://www.hakank.org/sicstus/arch_friends.pl
  * ECLiPSe   : http://www.hakank.org/eclipse/arch_friends.ecl

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


class ArchFriends : public Script {
protected:

  static const int n = 4;

  IntVarArray shoes;
  IntVarArray shops;

public:

  ArchFriends(const Options& opt) 
    : 
    shoes(*this, n, 0, n-1),
    shops(*this, n, 0, n-1)
  {

    IntVar
      ecru_espadrilles(shoes[0]), 
      fuchsia_flats(shoes[1]), 
      purple_pumps(shoes[2]), 
      suede_sandals(shoes[3]);

    IntVar
      Foot_Farm(shops[0]), 
      Heels_in_a_Handcart(shops[1]), 
      The_Shoe_Palace(shops[2]), 
      Tootsies(shops[3]);


    distinct(*this, shoes, opt.icl());
    distinct(*this, shops, opt.icl());

    // 1. Harriet bought fuchsia flats at Heels in a Handcart.
    rel(*this, fuchsia_flats == Heels_in_a_Handcart);

    // 2. The store she visited just after buying her purple pumps was 
    //    not Tootsies.
    rel(*this, purple_pumps + 1 != Tootsies);

    // 3. The Foot Farm was Harriet's second stop.
    rel(*this, Foot_Farm == 1);

    // 4. Two stops after leaving The Shoe Place, Harriet 
    //    bought her suede sandals.
    rel(*this, The_Shoe_Palace + 2 == suede_sandals);

    // branching
    branch(*this, shops, INT_VAR_NONE(), INT_VAL_MIN());
    branch(*this, shoes, INT_VAR_NONE(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "shops : " << shoes << endl;
    os << "shops : " << shops << endl;
    os << endl;

  }


  // Constructor for cloning s
  ArchFriends(bool share, ArchFriends& s) : Script(share,s) {
    shops.update(*this, share, s.shops);
    shoes.update(*this, share, s.shoes);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new ArchFriends(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("ArchFriends");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<ArchFriends,DFS,Options>(opt);
    
  return 0;
}


