/*

  Cross word in Comet.

  This is a standard example for constraint logic programming. See e.g.
 
  http://www.cis.temple.edu/~ingargio/cis587/readings/constraints.html
  """
  We are to complete the puzzle
 
       1   2   3   4   5
     +---+---+---+---+---+       Given the list of words:
   1 | 1 |   | 2 |   | 3 |             AFT     LASER
     +---+---+---+---+---+             ALE     LEE
   2 | # | # |   | # |   |             EEL     LINE
     +---+---+---+---+---+             HEEL    SAILS
   3 | # | 4 |   | 5 |   |             HIKE    SHEET
     +---+---+---+---+---+             HOSES   STEER
   4 | 6 | # | 7 |   |   |             KEEL    TIE
     +---+---+---+---+---+             KNOT
   5 | 8 |   |   |   |   |
     +---+---+---+---+---+       
   6 |   | # | # |   | # |       The numbers 1,2,3,4,5,6,7,8 in the crossword
     +---+---+---+---+---+       puzzle correspond to the words 
                                                   that will start at those locations.
  """

  The model was inspired by Sebastian Brand's Array Constraint cross word example
  http://www.cs.mu.oz.au/~sbrand/project/ac/
  http://www.cs.mu.oz.au/~sbrand/project/ac/examples.pl


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/crossword.mzn
  * Comet   : http://www.hakank.org/comet/crossword.co


  Note 2009-10-13: This version uses the new (in version 3.2) 
                   version of element which handles Matrices.

                   The older code is kept for comparison.
                   Compare with:
                   http://www.hakank.org/comet/crossword.cpp


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

/*
  Solution: 
  E[0]: 0 hoses
  E[1]: 2 sails
  E[2]: 4 steer
  E[3]: 6 hike
  E[4]: 7 keel
  E[5]: 11 ale
  E[6]: 13 lee
  E[7]: 1 laser
  
 */


#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

#include <string>

using namespace Gecode;

enum alpha {zzz, a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z};
std::string az[] = {"", "a","b","c","d","e","f","g","h","i","j","k","l","m","n","o",
                    "p","q","r","s","t","u","v","w","x","y","z"};

// Definition of the words. A zero is used to fill the row.
static const int _A[] = 
  {
    h, o, s, e, s, //  HOSES
    l, a, s, e, r, //  LASER
    s, a, i, l, s, //  SAILS
    s, h, e, e, t, //  SHEET
    s, t, e, e, r, //  STEER
    h, e, e, l, 0, //  HEEL
    h, i, k, e, 0, //  HIKE
    k, e, e, l, 0, //  KEEL
    k, n, o, t, 0, //  KNOT
    l, i, n, e, 0, //  LINE
    a, f, t, 0, 0, //  AFT
    a, l, e, 0, 0, //  ALE
    e, e, l, 0, 0, //  EEL
    l, e, e, 0, 0, //  LEE
    t, i, e, 0, 0  //  TIE
  };



/**
 *
 * Special version of element for an array version of a "matrix" words,
 * E is an integer variable array, C is an array of IntVars for
 * the offset j in the words "matrix".
 *
 * The call 
 *    element_offset(*this, words, E[i], word_len_v, C[j], res, opt.icl());
 *
 * corresponds to:
 *    res = words[E[i], j] --> words[E[i]*word_len+J]
 *
 */

// Note: This is not needed anymore. Thanks, Christian!
/*
void element_offset(Space& space,
                   IntArgs words,
                   IntVar e,
                   IntVar word_len,
                   IntVar c,
                   IntVar res,
                   IntConLevel icl = ICL_DOM) {

      element(space, words, 
              plus(space, 
                   mult(space, 
                        e, 
                        word_len, icl), 
                   c, icl), 
              res, icl);
}
*/

class Crossword : public Script {
protected:

  static const int num_words = 15; // number of words
  static const int word_len = 5;   // word length
  static const int n = 8;   // number of unknown overlappings

  //
  // E contains which word (position in A) to select for the overlappings.
  //
  IntVarArray E;

public:



  Crossword(const Options& opt) 
    : 
    E(*this, n, 0, num_words-1)
  {
 
    IntArgs A(num_words*word_len, _A);   

    // New: note A, columns, rows
    Matrix<IntArgs> A2(A, word_len, num_words);

    // The overlapping positions in the cross word, i.e.
    // where the letters is the same.
    int num_overlapping = 12;
    int _overlapping[] =
      {
       0,2,   1,0,   //  s   A[E[0], 2] = A[E[1], 0]
       0,4,   2,0,   //  s   etc
       
       3,1,   1,2,   //  i
       3,2,   4,0,   //  k
       3,3,   2,2,   //  e
       
       6,0,   1,3,   //  l
       6,1,   4,1,   //  e
       6,2,   2,3,   //  e
       
       7,0,   5,1,   //  l
       7,2,   1,4,   //  s
       7,3,   4,2,   //  e
       7,4,   2,4    //  r

      };
    
    IntArgs overlapping(num_overlapping*4, _overlapping);

    // convenience variable for the element constraints below
    IntVar word_len_v(*this, word_len, word_len);

    //
    // check all overlapping positions
    //
    for(int I = 0; I < num_overlapping; I++) {
      
      // A[E[overlapping[I,0]], overlapping[I,1]] ==  A[E[overlapping[I,2]], overlapping[I,3]]
      IntVar e1(*this, 0, num_overlapping*4);
      IntVar e2(*this, 0, num_overlapping*4);

      IntVarArray o(*this, 4, 0, num_overlapping*4);
      for(int J = 0; J < 4; J++) {
        rel(*this, o[J] == overlapping[I*4+J], opt.icl());
      }

      element(*this, E, o[0], e1, opt.icl());
      element(*this, E, o[2], e2, opt.icl());     

      IntVar a1(*this, 0, num_words*word_len);

      // Note: This is not needed anymore.
      // element_offset(*this, A, e1, word_len_v, o[1], a1, opt.icl());
      // element_offset(*this, A, e2, word_len_v, o[3], a1, opt.icl());

      // Note: This is the new version. Much easier!
      element(*this, A2, o[1], e1, a1, opt.icl());
      element(*this, A2, o[3], e2, a1, opt.icl());

    }

    //
    // Redundant constraint which don't make any difference for consistency level == ICL_DOM
    //
    distinct(*this, E, opt.icl());

    branch(*this, E, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    
    //
    // print the words 
    //
    try {
      for(int I = 0; I < n; I++) {
        os << "E[" << I << "]: " << E[I] << " ";
        for(int J = 0; J < word_len; J++) {
          
          os << az[_A[E[I].val()*word_len+J]]; 
        }
        os << std::endl;
      }
    } catch(Exception E) {
      os << E.what() << std::endl;
    }
        
  }


  // Constructor for cloning s
  Crossword(bool share, Crossword& s) : Script(share,s) {
    E.update(*this, share, s.E);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Crossword(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("Crossword");

  opt.solutions(0);
  opt.icl(ICL_DOM);

  opt.parse(argc,argv);
  Script::run<Crossword,DFS,Options>(opt);    

  return 0;
}
