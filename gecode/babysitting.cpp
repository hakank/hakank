/*
  
  Babysitting puzzle (Dell Logic Puzzles) in Gecode.

  Problem from http://brownbuffalo.sourceforge.net/BabysittingClues.html
  """
  Title: Babysitting
  Author: Scott Marley
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 7
  Stars: 1

  Each weekday, Bonnie takes care of five of the neighbors' children. 
  The children's names are Keith, Libby, Margo, Nora, and Otto; last 
  names are Fell, Gant, Hall, Ivey, and Jule. Each is a different
  number of years old, from two to six. Can you find each child's 
  full name and age?

  1. One child is named Libby Jule.
  2. Keith is one year older than the Ivey child, who is one year 
     older than Nora.
  3. The Fell child is three years older than Margo.
  4. Otto is twice as many years old as the Hall child.

  Determine: First name - Last name - Age 
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/babysitting.mzn
  * SICStus Prolog   : http://www.hakank.org/sicstus/babysitting.pl
  * ECLiPSe   : http://www.hakank.org/eclipse/babysitting.ecl

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


class Babysitting : public Script {
protected:

  static const int n = 5;

  IntVarArray last;
  IntVarArray age;

public:

  Babysitting(const Options& opt) 
    : 
    last(*this, n, 0, n-1),
    age(*this, n, 2, 6)
  {

    enum {
      Keith,
      Libby,
      Margo,
      Nora,
      Otto
    };

    IntVar
      Fell(last[0]), 
      Gant(last[1]), 
      Hall(last[2]), 
      Ivey(last[3]),
      Jule(last[4]);

    distinct(*this, last, opt.icl());
    distinct(*this, age, opt.icl());


    //  1. One child is named Libby Jule.
    rel(*this, Jule == Libby);

    // 2. Keith is one year older than the Ivey child, who is one 
    //    year older than Nora.
    rel(*this, 
        age[Keith] == element(age,Ivey) + 1 &&
        Keith != Ivey
        );   
    rel(*this, element(age,Ivey) == age[Nora] + 1 &&
        Ivey != Nora
        );

    //  3. The Fell child is three years older than Margo.
    rel(*this, element(age,Fell) == age[Margo] + 3 &&
        Fell != Margo
        );

    // 4. Otto is twice as many years old as the Hall child.
    rel(*this, age[Otto] == element(age,Hall)*2 &&
        Otto != Hall
        );

    // branching
    branch(*this, last, INT_VAR_NONE(), INT_VAL_MIN());
    branch(*this, age, INT_VAR_NONE(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "last: " << last << endl;
    os << "age : " << age << endl;
    os << endl;

  }


  // Constructor for cloning s
  Babysitting(bool share, Babysitting& s) : Script(share,s) {
    last.update(*this, share, s.last);
    age.update(*this, share, s.age);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Babysitting(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("Babysitting");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<Babysitting,DFS,Options>(opt);
    
  return 0;
}


