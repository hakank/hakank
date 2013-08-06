/*

  Stable marriage problem in Gecode.


  Translation of the OPL version from
  Pascal Van Hentenryck "The OPL Optimization Programming Language"
  Also:
  http://www.comp.rgu.ac.uk/staff/ha/ZCSP/additional_problems/stable_marriage/stable_marriage.pdf



  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/stable_marriage.mzn
  * Comet   : http://www.hakank.org/comet/stable_marriage.co


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/


/*

  Solutions:

  - Van Hentenryck's problem:
  wife   : [ 2  1  5  3  4  ]
  husband: [ 2  1  4  5  3  ]
  
  wife   : [ 2  3  5  1  4  ]
  husband: [ 4  1  2  5  3  ]
  
  wife   : [ 4  1  2  3  5  ]
  husband: [ 2  3  4  1  5  ]
  

  - MathWorld problem:  
  wife   : [ 6  4  9  8  3  7  1  5  2  ]
  husband: [ 7  9  5  2  8  1  6  4  3  ]
  
  wife   : [ 6  5  9  8  3  7  1  4  2  ]
  husband: [ 7  9  5  8  2  1  6  4  3  ]
  
  wife   : [ 6  1  4  8  5  7  3  2  9  ]
  husband: [ 2  8  7  3  5  1  6  4  9  ]
  
  wife   : [ 6  1  4  8  5  9  3  2  7  ]
  husband: [ 2  8  7  3  5  1  9  4  6  ]
  
  wife   : [ 6  4  1  8  5  7  3  2  9  ]
  husband: [ 3  8  7  2  5  1  6  4  9  ]
  
  wife   : [ 7  5  9  8  3  6  1  4  2  ]
  husband: [ 7  9  5  8  2  6  1  4  3  ]


 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;



/**
 *
 * "Matrix" variant for modelling the following constraint
 * where c is an IntArgs and y is IntVarArray
 *
 *   res = c[ix, y[ix]]
 *
 * Since c is an array this is: res = c[ix*n + y[ix]]
 *
 * e.g.  res = rankMen[m, wife[m]]
 *
 * c: IntArgs, array interpreted as matrix
 * y: IntVarArray, also interpreted as a matrix
 * n: row/col dimension, square "matrix" assumed
 * icl: consistency levels 
 *
 * res: IntVar (result)
 *
 */
IntVar element_m2(Space & space, IntArgs c, int ix, IntVarArray y, int n, IntConLevel icl = ICL_DOM) {
  IntVar ixx(space, ix, ix);  
  IntVar y_ix(space, 0, n*n);
  element(space, y, ixx, y_ix, icl);               // y_ix = y[ix]

  IntVar ix_n(space, 0, n*n);
  IntVar ix_n_plus_y_ix(space, 0, n*n);
  rel(space, ix_n == ixx * n , icl);              // ix_n = ix*n
  rel(space, ix_n_plus_y_ix == ix_n + y_ix, icl); // ix_n_plus_y = ix*n + y[ix]

  IntVar res(space, 0,n*n);
  element(space, c, ix_n_plus_y_ix, res, icl);     // res = c[ix*n + y[ix]]

  return res;

} // end element_m2


class Stable : public Script {
protected:

  // static const int n = 5; // for the Van Hentenryck example
  static const int n = 9; // for the Math World example

  IntVarArray wife;
  IntVarArray husband;

public:

  Stable(const Options& opt) 
  : 
    wife(*this, n, 0, n-1),
    husband(*this, n, 0, n-1)
  {


    // This is the example from Van Hentenryck OPL book
    /*
    int _rankWomen[] = 
      {

        1, 2, 4, 3, 5,
        3, 5, 1, 2, 4,
        5, 4, 2, 1, 3,
        1, 3, 5, 4, 2,
        4, 2, 3, 5, 1

      };

    int _rankMen[] = 
      {
        5, 1, 2, 4, 3,
        4, 1, 3, 2, 5,
        5, 3, 2, 4, 1,
        1, 5, 4, 3, 2,
        4, 3, 2, 1, 5

      };
    */

    /*
      Data from
      http://mathworld.wolfram.com/StableMarriageProblem.html

      Note: The solutions in this model is not the same as the web page.
      
    */
    int _rankWomen[] = 
      {
        3, 1, 5, 2, 8, 7, 6, 9, 4,
        9, 4, 8, 1, 7, 6, 3, 2, 5,
        3, 1, 8, 9, 5, 4, 2, 6, 7,
        8, 7, 5, 3, 2, 6, 4, 9, 1,
        6, 9, 2, 5, 1, 4, 7, 3, 8,
        2, 4, 5, 1, 6, 8, 3, 9, 7,
        9, 3, 8, 2, 7, 5, 4, 6, 1,
        6, 3, 2, 1, 8, 4, 5, 9, 7,
        8, 2, 6, 4, 9, 1, 3, 7, 5
      };

    int _rankMen[] = 
      {
        7, 3, 8, 9, 6, 4, 2, 1, 5,
        5, 4, 8, 3, 1, 2, 6, 7, 9,
        4, 8, 3, 9, 7, 5, 6, 1, 2,
        9, 7, 4, 2, 5, 8, 3, 1, 6,
        2, 6, 4, 9, 8, 7, 5, 1, 3,
        2, 7, 8, 6, 5, 3, 4, 1, 9,
        1, 6, 2, 3, 8, 5, 4, 9, 7,
        5, 6, 9, 1, 2, 8, 4, 3, 7,
        6, 1, 4, 7, 5, 8, 3, 9, 2
      };

    IntArgs rankWomen(n*n, _rankWomen);
    IntArgs rankMen(n*n, _rankMen);

    // Note: The comments below are the code from the Comet model
    //       http://www.hakank.org/comet/stable_marriage2.co

    /* forall(m in Men)
         cp.post(husband[wife[m]] == m);
    */
    for(int m = 0; m < n; m++) {
      rel(*this, element(husband,wife[m]) == m);
    }


    /*
     forall(w in Women)
       cp.post(wife[husband[w]] == w);
    */
    for(int w = 0; w < n; w++) {
      rel(*this, element(wife, husband[w]) == w);
    }

    /*
      forall(m in Men, w in Women)
        cp.post(rankMen[m,w] < rankMen[m, wife[m]] => rankWomen[w,husband[w]] < rankWomen[w,m]);
    */  
    for(int m = 0; m < n; m++) {
      IntVar rankMen_res = element_m2(*this, rankMen, m, wife, n, opt.icl());
      for(int w = 0; w < n; w++) {
        IntVar rankWomen_res = element_m2(*this, rankWomen, w, husband, n, opt.icl());
        rel(*this, (rankMen[m*n+w] < rankMen_res) >> (rankWomen_res < rankWomen[w*n+m]));
      }
    }

    /*
      forall(w in Women, m in Men)
         cp.post(rankWomen[w,m] < rankWomen[w,husband[w]] => rankMen[m,wife[m]] < rankMen[m,w]);
    */
    for(int w = 0; w < n; w++) {
      IntVar rankWomen_res = element_m2(*this, rankWomen, w, husband, n, opt.icl());
      for(int m = 0; m < n; m++) {
        IntVar rankMen_res = element_m2(*this, rankMen, m, wife, n, opt.icl());
        rel(*this, (rankWomen[w*n+m] < rankWomen_res) >> (rankMen_res < rankMen[m*n+w]));
      }
    }

    channel(*this, wife, husband);


    branch(*this, wife, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 
    branch(*this, husband, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 

  }


  // Print solution
  virtual void
  print(std::ostream& os) const {

    // Note: Adjust for presentation.
    os << "wife   : [ ";
    for(int i = 0; i < n; i++) {
      os << wife[i].val() + 1 << "  ";
    }
    os << "]" << std::endl;
    os << "husband: [ ";
    for(int i = 0; i < n; i++) {
      os << husband[i].val() + 1 << "  ";
    }
    os << "]" << std::endl;
    os << std::endl;
  }

  // Constructor for cloning s
  Stable(bool share, Stable& s) : Script(share,s) {
    wife.update(*this, share, s.wife);
    husband.update(*this, share, s.husband);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Stable(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("Stable");
  opt.solutions(0);
  opt.icl(ICL_DOM);
  opt.parse(argc,argv);
  Script::run<Stable,DFS,Options>(opt);

  return 0;

}


