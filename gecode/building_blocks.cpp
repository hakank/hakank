/*

  Building Blocks puzzle in Gecode.


  From http://brownbuffalo.sourceforge.net/BuildingBlocksClues.html
  """
  Each of four alphabet blocks has a single letter of the alphabet on each 
  of its six sides. In all, the four blocks contain every letter but 
  Q and Z. By arranging the blocks in various ways, you can spell all of 
  the words listed below. Can you figure out how the letters are arranged 
  on the four blocks?

  BAKE ONYX ECHO OVAL
 
  GIRD SMUG JUMP TORN
 
  LUCK VINY LUSH WRAP
  """

  Compare with the following model:
  * Comet   : http://www.hakank.org/comet/building_blocks.co

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include "gecode/minimodel.hh"

using namespace Gecode;

/*
  The first solution is

  Block: Letters
      1: A C D J N S
      2: B F I O U W
      3: E G L P T Y
      4: H K M R V X

 */


class BuildingBlocks : public Script {
protected:

  static const int n = 4;          // number of blocks
  static const int num_words = 12; // number of words
  static const int sides = 6;      // sides per block

  // position on of each letter
  IntVarArray block;

  // letters
  enum LETTERS {A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,R,S,T,U,V,W,X,Y};

public:

  // Actual model
  BuildingBlocks(const SizeOptions& opt) : 
    block(*this, n*sides, 1, n)
  {
    
    // the words to place on the block
    int words[] = 
      {
        B,A,K,E,
        O,N,Y,X,
        E,C,H,O,
        O,V,A,L,
        G,I,R,D,
        S,M,U,G,
        J,U,M,P,
        T,O,R,N,
        L,U,C,K,
        V,I,N,Y,
        L,U,S,H,
        W,R,A,P
      };


    Matrix<IntVarArray> block_m(block, sides, n);


    // the letters in a word must be on a different die
    for(int r = 0; r < num_words; r++) {
      IntVarArray d(*this, n, 1, n); // temporary array for distinct
      for(int i = 0; i < n; i++) {
        // int t = words[r*n+i];
        // element(*this, block, IntVar(*this, t, t), d[i], opt.icl());
        // neater:
        rel(*this, element(block, words[r*n+i]) == d[i]);
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
  BuildingBlocks(bool share, BuildingBlocks& s) : Script(share,s) {
    block.update(*this, share, s.block);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new BuildingBlocks(share,*this);
  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    std::string LETTERS_str = "ABCDEFGHIJKLMNOPRSTUVWXY";

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
  SizeOptions opt("BuildingBlocks");
  opt.solutions(0);
  opt.icl(ICL_DOM);
  opt.parse(argc,argv);

  Script::run<BuildingBlocks,DFS,SizeOptions>(opt);

  return 0;
}


