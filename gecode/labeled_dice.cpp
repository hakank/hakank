/*

  Labeled dice puzzle in Gecode.

  From Jim Orlin "Colored letters, labeled dice: a logic puzzle"
  http://jimorlin.wordpress.com/2009/02/17/colored-letters-labeled-dice-a-logic-puzzle/
  """
  My daughter Jenn bough a puzzle book, and showed me a cute puzzle.  There 
  are 13 words as follows:  BUOY, CAVE, CELT, FLUB, FORK, HEMP, JUDY, 
  JUNK, LIMN, QUIP, SWAG, VISA, WISH.

  There are 24 different letters that appear in the 13 words.  The question 
  is:  can one assign the 24 letters to 4 different cubes so that the 
  four letters of each word appears on different cubes.  (There is one 
  letter from each word on each cube.)  It might be fun for you to try 
  it.  I'll give a small hint at the end of this post. The puzzle was 
  created by Humphrey Dudley.
  """


  Compare with the following models:
  * Comet   : http://www.hakank.org/comet/labeled_dice.co
  * Gecode  : http://www.hakank.org/gecode/building_blocks.cpp


  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

 */


/*
  First solution:

  The dies:
  1: A H J L O Q
  2: C F N P S Y
  3: B D E G I K
  4: M R T U V W

 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include "gecode/minimodel.hh"

using namespace Gecode;

class LabelingDice : public Script {
protected:

  static const int n = 4;          // number of blocks
  static const int num_words = 13; // number of words
  static const int sides = 6;      // sides per block

  // position on of each letter
  IntVarArray block;

  // letters
  enum LETTERS {A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,Y};

public:

  // Actual model
  LabelingDice(const SizeOptions& opt) : 
    block(*this, n*sides, 1, n)
  {
    
    // the words to place on the block
    int words[] = 
      {
        B,U,O,Y,
        C,A,V,E, 
        C,E,L,T, 
        F,L,U,B, 
        F,O,R,K, 
        H,E,M,P, 
        J,U,D,Y, 
        J,U,N,K, 
        L,I,M,N, 
        Q,U,I,P, 
        S,W,A,G, 
        V,I,S,A, 
        W,I,S,H
      };


    Matrix<IntVarArray> block_m(block, sides, n);


    // the letters in a word must be on a different die
    for(int r = 0; r < num_words; r++) {
      IntVarArray d(*this, n, 1, n); // temporary array for distinct
      for(int i = 0; i < n; i++) {
        int t = words[r*n+i];
        element(*this, block, IntVar(*this, t, t), d[i], opt.icl());
      }
      distinct(*this, d, opt.icl());
    }

    // there must be exactly sides (6) letters of each die
    for(int i = 1; i <= n; i++) {
      count(*this, block, i, IRT_EQ, sides, opt.icl());
    }
    
    branch(*this, block, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Constructor for cloning s
  LabelingDice(bool share, LabelingDice& s) : Script(share,s) {
    block.update(*this, share, s.block);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new LabelingDice(share,*this);
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    std::string LETTERS_str = "ABCDEFGHIJKLMNOPQRSTUVWY";
    os << "Letters: " << std::endl;
    for(int i = 0; i < n*sides; i++) {
      os << LETTERS_str[i] << ": " << block[i] << std::endl;
    }
    os << std::endl;

    os << "The dies: "<< std::endl;
    for(int d = 1; d <= n; d++) {
      os << d << ": ";
      for(int i = 0; i < n*sides; i++) 
        if (block[i].val() == d) {
          os << LETTERS_str[i] << " ";
        }
      os << std::endl;
    }
    os << std::endl;


  }

};

/** 
 *  main
 */
int
main(int argc, char* argv[]) {
  SizeOptions opt("LabelingDice");
  opt.solutions(0);
  opt.icl(ICL_DOM);
  opt.parse(argc,argv);

  Script::run<LabelingDice,DFS,SizeOptions>(opt);

  return 0;
}


