/*
  
  Exodus puzzle (Dell Logic Puzzles) in Gecode.

  From 
   http://brownbuffalo.sourceforge.net/ExodusClues.html
   """
   Title: Exodus
   Author: Sophy McHannot
   Publication: Dell Logic Puzzles
   Issue: April, 1998
   Page: 14
   Stars: 2

   In preparation for Passover, five children at Hebrew school 
   (Bernice,Carl,Debby,Sammy, and Ted) 
   have been chosen to present
   different parts of the story of the Exodus from Egypt 
    (burning bush, captivity,
     Moses's youth, Passover, or the Ten Commandments). 
   Each child is a different age 
     (three, five, seven, eight, or ten), 
   and the family of each child has recently made its own exodus 
   to America from a different country 
   (Ethiopia, Kazakhstan, Lithuania, Morocco, or Yemen). 
   Can you find the age of each child, his or her family's country of 
   origin, and the part of the Exodus story each related?

    1. Debby's family is from Lithuania.
    2. The child who told the story of the Passover is two years older
       than Bernice.
    3. The child whose family is from Yemen is younger than the child from
       the Ethiopian family.
    4. The child from the Moroccan family is three years older than Ted.
    5. Sammy is three years older than the child who told the story of
       Moses's youth in the house of the Pharaoh.
    6. Carl related the story of the captivity of the Israelites in Egypt.
    7. The five-year-old child told the story of the Ten Commandments.
    8. The child who told the story of the burning bush is either two or
       three years older than the one whose family came from
       Kazakhstan.

   Determine: Age -- Child -- Country -- Story
   """

  Compare with the following models:
  * ECLiPSE: http://www.hakank.org/eclipse/exodus.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/exodus.pl

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


class Exodus : public Script {
protected:

  static const int n=5;

  IntVarArray Story;
  IntVarArray Age;
  IntVarArray Country;

public:

  Exodus(const Options& opt) 
    : 
    Story(*this, n, 0, n-1),
    Age(*this, n, 3, 10),  
    Country(*this, n, 0, n-1)
  {

    // the ages are 3,5,7,8,10
    // so we restrict ages to this domain
    IntArgs _ages(5, 3,5,7,8,10);
    IntSet dom_ages(_ages);
    dom(*this, Age, dom_ages);
    
    enum {
      Bernice,
      Carl,
      Debby,
      Sammy,
      Ted
    };

    IntVar
      BurningBush(Story[0]), 
      Captivity(Story[1]), 
      MosessYouth(Story[2]), 
      Passover(Story[3]),
      TenCommandments(Story[4]);


    IntVar
      Ethiopia(Country[0]), 
      Kazakhstan(Country[1]), 
      Lithuania(Country[2]), 
      Morocco(Country[3]), 
      Yemen(Country[4]);

    distinct(*this, Story);
    distinct(*this, Age);
    distinct(*this, Country);


    rel(*this, Debby == Lithuania);
    rel(*this, element(Age,Passover) == Age[Bernice] + 2);
    rel(*this, element(Age,Yemen) < element(Age,Ethiopia));
    rel(*this, element(Age,Morocco) == Age[Ted] + 3);
    rel(*this, Age[Sammy] == element(Age,MosessYouth) + 3);
    rel(*this, Carl == Captivity);
    rel(*this, element(Age,TenCommandments) == 5);
    rel(*this,
        (element(Age,BurningBush) == element(Age,Kazakhstan) + 2)
         ||
        (element(Age,BurningBush) == element(Age,Kazakhstan) + 3)
        );

    
    // branching
    branch(*this, Age, INT_VAR_SIZE_MIN(), INT_VAL_SPLIT_MIN());
    branch(*this, Story, INT_VAR_SIZE_MIN(), INT_VAL_SPLIT_MIN());
    branch(*this, Country, INT_VAR_SIZE_MIN(), INT_VAL_SPLIT_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "Story  : " << Story << endl;
    os << "Age    : " << Age << endl;
    os << "Country: " << Country << endl;
  }


  // Constructor for cloning s
  Exodus(bool share, Exodus& s) : Script(share,s) {
    Story.update(*this, share, s.Story);
    Age.update(*this, share, s.Age);
    Country.update(*this, share, s.Country);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Exodus(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("Exodus");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<Exodus,DFS,Options>(opt);
    
  return 0;
}


