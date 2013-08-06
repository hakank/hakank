/*

  Who killed agatha? (The Dreadsbury Mansion Murder Mystery) in Gecode.
 
  This is a standard benchmark for theorem proving.
 
  http://www.lsv.ens-cachan.fr/~goubault/H1.dist/H1.1/Doc/h1003.html  
  """ 
  Someone in Dreadsbury Mansion killed Aunt Agatha. 
  Agatha, the butler, and Charles live in Dreadsbury Mansion, and 
  are the only ones to live there. A killer always hates, and is no 
  richer than his victim. Charles hates noone that Agatha hates. Agatha 
  hates everybody except the butler. The butler hates everyone not richer 
  than Aunt Agatha. The butler hates everyone whom Agatha hates. 
  Noone hates everyone. Who killed Agatha? 
  """

  Originally from 
  F. J. Pelletier: Seventy-five problems for testing automatic theorem provers.
  Journal of Automated Reasoning, 2: 191-216, 1986.

  
  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/who_killed_agatha.mzn
  * Comet: http://www.hakank.org/comet/who_killed_agatha.co
  * Gecode: http://www.hakank.org/gecode/who_killed_agatha2.cpp


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

/** 
 * Result: 8 solutions which all gives the same result:
 *         Agatha killed herself.
 */


/**
 *
 * "Matrix" variant of the element constraint.
 *
 * where m is an IntVarArray interpreted as a matrix 
 *
 *   m[x*n + y] = val         (which should be interpreted as m'[x, y] = val)
 *
 * m: the IntVarArray array
 * x, y: two IntVars
 * rows: row dimension
 * cols: col dimension
 * val: integer value to set
 * icl: consistency levels 
 *
 */
void element_m(Space & space, IntVarArray m, IntVar x, IntVar y, int rows, int cols, int val, IntConLevel icl = ICL_DOM) {

  IntVar x_n(space, 0, (rows*cols)-1);
  IntVar ix(space, 0, (rows*cols)-1); 
  IntVar e1(space, 0, rows-1);
  rel(space, x_n == x*rows, icl); // x_n = x*rows
  rel(space, ix==x_n+y, icl);     // ix = x*row + y
  element(space, m, ix, e1, icl);  // e1 = m[x*row +y]
  rel(space, e1 == val, icl);     // val = m[x*row +y]

} // end element_m


class Agatha : public Script {
protected:

  static const int n = 3;      // length of array

  IntVar the_killer;
  IntVar the_victim;

public:

  Agatha(const Options& opt) 
  : 
    the_killer(*this, 0, n-1),
    the_victim(*this, 0, n-1)
  {

    int agatha  = 0;
    int butler  = 1;
    int charles = 2;

    IntVarArray hates(*this, n*n, 0, 1);
    IntVarArray richer(*this, n*n, 0, 1);
 
    // Note that the order in the matrices hates_m and richer_m in Gecode are
    // m(column, row)    
    Matrix<IntVarArray> hates_m(hates, n, n);
    Matrix<IntVarArray> richer_m(richer, n, n);

    //
    // The comments below contains the corresponding MiniZinc code,
    // for documentation and comparision.
    //

    // """
    // Agatha, the butler, and Charles live in Dreadsbury Mansion, and 
    // are the only ones to live there. 
    // """


    // "A killer always hates, and is no richer than his victim."
    // MiniZinc: hates[the_killer, the_victim] = 1
    // We translate this to an element expression in the array hates:
    //   hates[the_killer*n + the_victim]
    element_m(*this, hates, the_killer, the_victim, n, n, 1, opt.icl());

    // MiniZinc: richer[the_killer, the_victim] == 0
    // we use exactly the same principle here as above
    element_m(*this, richer, the_killer, the_victim, n, n, 0, opt.icl());


    // define the concept of richer: no one is richer than him-/herself
    for(int i = 0; i < n; i++) {
      rel(*this, richer_m(i,i), IRT_EQ, 0, opt.icl());
    }

    // (contd...) if i is richer than j then j is not richer than i
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        if (i != j) {
          // MiniZinc: richer[i,j] == 1 <-> richer[j,i] == 0
          rel(*this, (richer_m(j,i) == 1) == (richer_m(i,j) == 0), opt.icl());
        }
      }
    }
  
   // "Charles hates noone that Agatha hates." 
    for(int i = 0; i < n; i++) {
      // MiniZinc: hates[charles, i] = 0 <- hates[agatha, i] = 1
      rel(*this, (hates_m(i,agatha) == 1) >> (hates_m(i,charles) == 0), opt.icl());
    }

    // "Agatha hates everybody except the butler. "
    rel(*this, hates_m(charles, agatha) == 1, opt.icl()) ;
    rel(*this, hates_m(agatha , agatha) == 1, opt.icl());
    rel(*this, hates_m(butler , agatha) == 0, opt.icl());

   // "The butler hates everyone not richer than Aunt Agatha. "
    for(int i = 0; i < n; i++) {
      // MiniZinc: hates[butler, i] = 1 <- richer[i, agatha] = 0
      rel(*this, ((richer_m(agatha,i) == 0) >> (hates_m(i,butler) == 1)), opt.icl());
    }

    // "The butler hates everyone whom Agatha hates." 
    for(int i = 0; i < n; i++) {
      // MiniZinc: hates[butler, i] = 1 <- hates[agatha, i] = 1
      rel(*this, (hates_m(i, agatha) == 1) >> (hates_m(i, butler) == 1), opt.icl()); 
    }

    // "No one hates everyone. "
    for(int i = 0; i < n; i++) {
      // MiniZinc: sum(j in r) (hates[i,j]) <= 2
      linear(*this, hates_m.row(i), IRT_LQ, 2, opt.icl());
      
    }

    // "Who killed Agatha?"
    rel(*this, the_victim == agatha, opt.icl());

    branch(*this, hates, INT_VAR_NONE(), INT_VAL_MIN()); 
    branch(*this, richer, INT_VAR_NONE(), INT_VAL_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "the_victim: " << the_victim << std::endl;
    os << "the_killer: " << the_killer << std::endl;
    os << std::endl;
  }

  // Constructor for cloning s
  Agatha(bool share, Agatha& s) : Script(share,s) {
    the_killer.update(*this, share, s.the_killer);
    the_victim.update(*this, share, s.the_victim);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Agatha(share,*this);
  }
};


int
main(int argc, char* argv[]) {
  Options opt("Agatha");
  opt.solutions(0);
  opt.parse(argc,argv);
  Script::run<Agatha,DFS,Options>(opt);
  return 0;
}


