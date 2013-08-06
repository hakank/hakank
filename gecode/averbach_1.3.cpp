/*
  
  Recreational mathematics in Gecode.

  Problem 1.3 from 
  Averbach & Chein "Problem Solving Through Recreational Mathematics":
  """
  Armand Alloway, Basil Bennington, Col. Carton Cunningham, Durwood Dunstan, and 
  Everitt Elmsby, Esq are the five senior members of the Devonshire Polo Club. 
  Each owns a pony that is named of the wife of one of the others.
  
  - Mr Alloway's pony is named Geogette; 
  - Col Cunningham owns Jasmine
  - Mr Elmsby owns Inez
  - Francine, owned by Mr Dunstan is named after Alloways wife
  - Georgettes husband owns the pony that is named after Mr Bennington's wife
  - Helene Cunningham is the only wife who knows how to ride a horse.
  
  Who is Jasmine's husband? Who owns Helene?
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/averbach_1.3.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/averbach_1.3.pl
  * ECLiPSe: http://www.hakank.org/eclipse/averbach_1.3.ecl

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

void rightTo(Space& space, IntVar x, IntVar y) {
  rel(space, (x == y + 1) || (x == y -2) );
}

void leftTo(Space& space, IntVar x, IntVar y) {
  rightTo(space, y,x);
}


class Averbach : public Script {
protected:

  static const int n = 5;


  IntVarArray wife;
  IntVarArray pony;

public:

  Averbach(const Options& opt) 
    : 
    wife(*this, n, 0, n-1),
    pony(*this, n, 0, n-1)
  {

    // the men
    enum {
      Alloway,
      Bennington,
      Cunningham,
      Dunstan,
      Elmsby
    };

    // the name of the wifes and the names
    // of the ponies

    enum {
      Francine,
      Georgette,
      Helene,
      Inez,
      Jasmine
    };


    distinct(*this, wife);
    distinct(*this, pony);

    /*
      Wife and pony don't have the same name
    */
    
    for(int i = 0; i < n; i++) {
      rel(*this, pony[i] != wife[i]);
    }

    /*
      Mr Alloway's pony is named Geogette; 
    */
    rel(*this, pony[Alloway] == Georgette);
    rel(*this, wife[Alloway] != Georgette);
    

    /*
      Col Cunningham owns Jasmine
    */
    rel(*this, pony[Cunningham] == Jasmine);
    rel(*this, wife[Cunningham] != Jasmine);

    /*
      Mr Elmsby owns Inez
    */
    rel(*this, pony[Elmsby] == Inez);
    rel(*this, wife[Elmsby] != Inez);


    /*
      Francine, owned by Mr Dunstan is named after Alloways wife
    */
    rel(*this, pony[Dunstan] == Francine);
    rel(*this, wife[Alloway] == Francine);

    /*
      Georgettes husband owns the pony that is named after Mr Bennington's wife
      Translates into:
      "There is an X such that X is Georgette's husband and X owns a pony
      with the same name as Bennington's wife."
    */
    IntVar X(*this, 0, n-1);    
    rel(*this, element(wife,X) == Georgette);
    rel(*this, element(pony, X) == wife[Bennington]);


    /*
      Helene Cunningham is the only wife who knows how to ride a horse.
    */
    rel(*this, wife[Cunningham] == Helene);

    // branching
    branch(*this, wife, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    branch(*this, pony, INT_VAR_SIZE_MIN(), INT_VAL_MIN());


  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "wife: " << wife << endl;
    os << "pony: " << pony << endl;
    os << endl;

    string men_string[] = {"Alloway","Bennington","Cunningham","Dunstan","Elmsby"};
    string wife_pony_string[] = {"Francine","Georgette","Helene","Inez","Jasmine"};

    cout << setw(11) << "Men\t" << setw(11) << "Pony\t" << setw(11) << "Wife" << endl;
    for(int i = 0; i < n; i++) {
      cout << setw(11) << men_string[i] << "\t";

      for(int p = 0; p < 5; p++) {
        if (p == pony[i].val()) {
          cout << setw(11) << wife_pony_string[p] << "\t";
        }
      }

      for(int w = 0; w < 5; w++) {
        if (w == wife[i].val()) {
          cout << setw(11) << wife_pony_string[w] << endl;
        }
      }
    }

  }


  // Constructor for cloning s
  Averbach(bool share, Averbach& s) : Script(share,s) {
    wife.update(*this, share, s.wife);
    pony.update(*this, share, s.pony);

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

  opt.parse(argc,argv);

  Script::run<Averbach,DFS,Options>(opt);
    
  return 0;
}


