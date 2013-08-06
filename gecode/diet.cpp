/*

  Diet: Standard Operations Research example in Minizinc

  Minimize the cost for the products:
  Type of                        Calories   Chocolate    Sugar    Fat
  Food                                      (ounces)     (ounces) (ounces)
  Chocolate Cake (1 slice)       400           3            2      2
  Chocolate ice cream (1 scoop)  200           2            2      4
  Cola (1 bottle)                150           0            4      1
  Pineapple cheesecake (1 piece) 500           0            4      5


  See my other models:
  MiniZinc: http://www.hakank.org/minizinc/diet1.mzn
  Comet: http://www.hakank.org/comet/diet.co
  JaCoP: http://www.hakank.org/JaCoP/Diet.java
  Choco: http://www.hakank.org/choco/Diet.java
  Gecode/R: http://www.hakank.org/gecode_r/diet.rb


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

class Diet : public MinimizeScript {
protected:

  IntVarArray x;          // amount of each product
  IntVar t_cost;          // total cost to minimize
  static const int n = 4; // number of products
public:

  // search engines
  enum {
    SEARCH_BAB,
  }
;

  Diet(const Options& opt) 
    : 
    x(*this, n, 0, 1000),
    t_cost(*this, 0, 1000)
  {

    //  data

    int _price[]  = { 50, 20, 30, 80}; // in cents
    int limits[]  = {500,  6, 10,  8}; // requirements for each nutrition type

    // nutritions for each product
    int _calories[]  = {400, 200, 150, 500};
    int _chocolate[] = {  3,   2,   0,   0};
    int _sugar[]     = {  2,   2,   4,   4};
    int _fat[]       = {  2,   4,   1,   5};

    IntArgs price(n, _price);
    IntArgs calories(n, _calories);
    IntArgs chocolate(n, _chocolate);
    IntArgs sugar(n, _sugar);
    IntArgs fat(n, _fat);

    linear(*this, calories,  x, IRT_GQ, limits[0], opt.icl());
    linear(*this, chocolate, x, IRT_GQ, limits[1], opt.icl());
    linear(*this, sugar,     x, IRT_GQ, limits[2], opt.icl());
    linear(*this, fat,       x, IRT_GQ, limits[3], opt.icl());

    linear(*this, price, x, IRT_EQ, t_cost, opt.icl()); 

    branch(*this, x, INT_VAR_NONE(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "\t" << x << " t_cost: " << t_cost << std::endl;
  }

  // Return cost
  virtual IntVar cost(void) const {
    return t_cost;
  }

  // Constructor for cloning s
  Diet(bool share, Diet& s) : MinimizeScript(share,s) {
    x.update(*this, share, s.x);
    t_cost.update(*this, share, s.t_cost);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Diet(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  Options opt("Diet");
  opt.solutions(0);
  opt.iterations(20000);
  opt.search(Diet::SEARCH_BAB);
  opt.search(Diet::SEARCH_BAB, "bab");

  opt.parse(argc,argv);
  MinimizeScript::run<Diet,BAB,Options>(opt);

  return 0;
}


