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


  Note 2009-10-13: This version uses the new (in version 3.2) 
                   version of element which handles Matrices.

                   The older code is kept for comparison.
                   Compare with:
                   http://www.hakank.org/comet/who_killed_agatha.cpp

  Note 2010-05-30: Version 3.4.0 removed the tt(), imp(), ~ etc and
                   the code is not neater. The old version has been
                   kept as comment for comparison.

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
    // Note: This is the older variant. 
    // Matrix<IntVarArray> hates_m(hates, n, n);
    // Matrix<IntVarArray> richer_m(richer, n, n);

    Matrix<IntVarArgs> hates_m2(hates, n, n);
    Matrix<IntVarArgs> richer_m2(richer, n, n);

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
    // element_m(*this, hates, the_killer, the_victim, n, n, 1, opt.icl());

    // Note: This is the new version. The arguments are as follows:
    // See http://www.gecode.org/doc/3.2.0/reference/classGecode_1_1Matrix.html
    // element (Space &home, const Matrix< IntVarArgs > &m, IntVar x, IntVar y, IntVar z, IntConLevel icl=ICL_DEF)
    // element(*this, hates_m2, the_victim, the_killer , IntVar(*this,1,1), opt.icl());
    // 
    // Using the element expression:
    rel(*this, element(hates,the_killer*n+the_victim) == 1, opt.icl());

    // MiniZinc: richer[the_killer, the_victim] == 0
    // we use exactly the same principle here as above
    // element_m(*this, richer, the_killer, the_victim, n, n, 0, opt.icl());
    
    // Note: See above.
    // element(*this, richer_m2, the_victim, the_killer, IntVar(*this,0,0), opt.icl());
    // Using the element expression:
    rel(*this, element(richer, the_killer*n+the_victim) == 0, opt.icl());

    // define the concept of richer: no one is richer than him-/herself
    for(int i = 0; i < n; i++) {
      // rel(*this, richer_m2(i,i), IRT_EQ, 0, opt.icl());
      rel(*this, richer_m2(i,i) == 0, opt.icl());
    }

    // (contd...) if i is richer than j then j is not richer than i
    for(int i = 0; i < n; i++) {
      for(int j = 0; j < n; j++) {
        if (i != j) {
          // MiniZinc: richer[i,j] == 1 <-> richer[j,i] == 0
          rel(*this, (richer_m2(j,i) == 1) == (richer_m2(i,j) == 0), opt.icl());
        }
      }
    }
  
   // "Charles hates noone that Agatha hates." 
    for(int i = 0; i < n; i++) {
      // MiniZinc: hates[charles, i] = 0 <- hates[agatha, i] = 1
      rel(*this, (hates_m2(i,agatha) == 1) >> (hates_m2(i,charles) == 0), opt.icl());
    }

    // "Agatha hates everybody except the butler. "
    rel(*this, hates_m2(charles, agatha) == 1, opt.icl()) ;
    rel(*this, hates_m2(agatha , agatha) == 1, opt.icl());
    rel(*this, hates_m2(butler , agatha) == 0, opt.icl());

   // "The butler hates everyone not richer than Aunt Agatha. "
    for(int i = 0; i < n; i++) {
      // MiniZinc: hates[butler, i] = 1 <- richer[i, agatha] = 0
      rel(*this, ((richer_m2(agatha,i) == 0) >> (hates_m2(i,butler) == 1)), opt.icl());
    }

    // "The butler hates everyone whom Agatha hates." 
    for(int i = 0; i < n; i++) {
      // MiniZinc: hates[butler, i] = 1 <- hates[agatha, i] = 1
      rel(*this, (hates_m2(i, agatha) == 1) >> (hates_m2(i, butler) == 1), opt.icl()); 
    }

    // "No one hates everyone. "
    for(int i = 0; i < n; i++) {
      // MiniZinc: sum(j in r) (hates[i,j]) <= 2
      linear(*this, hates_m2.row(i), IRT_LQ, 2, opt.icl());
      
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


