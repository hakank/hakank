/**
 * Word square in Gecode.
 *
 *  From http://en.wikipedia.org/wiki/Word_square
 *  """
 *  A word square is a special case of acrostic. It consists of a set of words,
 *  all having the same number of letters as the total number of words (the
 *  "order" of the square); when the words are written out in a square grid
 *  horizontally, the same set of words can be read vertically.
 *  """
 *
 *  Compare with the following models:
 *  - http://www.hakank.org/comet/word_square.co
 *  - http://www.hakank.org/JaCoP/WordSquare.java
 *  - http://www.hakank.org/choco/WordSquare.java
 *  - http://www.hakank.org/minizinc/word_square.mzn
 *
 * Note 2009-10-13: This version uses the new (in version 3.2) 
 *                  version of element which handles Matrices.
 *
 *                  The older code is kept for comparison.
 *                  Compare with:
 *                  http://www.hakank.org/comet/word_square.cpp
 *
 *
 * Note 2009-10-21: Christian Schulte suggested
 *                  using branch strategy INT_VAL_SPLIT_MIN 
 *                  instead of INT_VAL_MAX .
 *                  This made an improvement for size 7 from
 *                  322 failures to 42 and from 1:16 minutes
 *                  to 10 seconds (for 1 solution).
 *                  
 *                  Now it manage to solve a size 8 in a reasonable 
 *                  time (1:33 minutes and 1018 failures):
 *                   abastral
 *                   bichrome
 *                   achiotes
 *                   shippers
 *                   troponin
 *                   rotenone
 *                   amerinds
 *                   lessness
 *
 * This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
 * Also, see my Gecode page: http://www.hakank.org/gecode/ .
 *
 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

#include <string>
#include <iostream>
#include <fstream>
#include <cctype>
#include <vector>

std::vector<std::string> _words; 
int num_words;

using namespace Gecode;


/**
 *
 * Special version of element for offset.
 *
 * The call 
 *    element_offset(*this, words, E[i], word_len_v, C[j], tmp, opt.icl());
 *
 * corresponds to:
 *    tmp = words[E[i], j] --> words[E[i]*word_len+J]
 *
 */

class WordSquare : public Script {
protected:

  int word_len;   // word length, from opt.size()

  // E contains which word (position in words) 
  // to select for the overlappings.
  IntVarArray E;

public:

  WordSquare(const SizeOptions& opt) 
    : 
    word_len(opt.size()),
    E(*this, word_len, 0, num_words-1)
  {
 
    std::cout << "num_words: " << num_words << std::endl;

    // copy the word list to IntArgs structure 
    // for using in element
    IntArgs words(num_words*word_len);
    for(int i = 0; i < num_words; i++) {
      for(int j = 0; j < word_len; j++) {
        words[i*word_len+j] = _words[i].at(j);
      }
    }

    Matrix<IntArgs> words_m(words, word_len, num_words);

    distinct(*this, E, ICL_DOM);

    //
    // check all overlapping positions
    //

    // convenience variables for the element constraints below
    // since element, plus, and mult wants IntVars.
    IntVar word_len_v(*this, word_len, word_len);
    IntVarArray C(*this, word_len, 0, word_len-1);
    for(int i = 0; i < word_len; i++) {
      rel(*this, C[i], IRT_EQ, i, opt.icl());
    }

    for(int i = 0; i < word_len; i++) {
      for(int j = 0; j < word_len; j++) {
        // words[E[i], j] ==  words[E[j],i]

        IntVar tmp(*this, 0, num_words);

        // words[E[i], j] --> words[E[i]*word_len+j] == tmp
        // element_offset(*this, words, E[i], word_len_v, C[j], tmp, opt.icl());

        // Note: This is the new version.
        element(*this, words_m, C[j], E[i], tmp, opt.icl());
        element(*this, words_m, C[i], E[j], tmp, opt.icl());

      }

    }

    // branch(*this, E, INT_VAR_SIZE_MIN, INT_VAL_MAX);
    // 2009-10-21: Christian Schulte suggested INT_VAL_SPLIT_MIN instead
    branch(*this, E, INT_VAR_SIZE_MIN(), INT_VAL_SPLIT_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {

    //
    // print the words 
    //
    os << "E: " << E << std::endl;

    try {

      os << "Square: " << std::endl;
      for(int i = 0; i < word_len; i++) {
        if (E[i].assigned()) {
          os << _words[E[i].val()] << std::endl;
        } else {
          os << "<unassigned>" << std::endl;
        } 
      }
    } catch(Exception e) {
      os << e.what() << std::endl;
    }
    os << std::endl;
        
  }


  // Constructor for cloning s
  WordSquare(bool share, WordSquare& s) : Script(share,s), word_len(s.word_len) {
    E.update(*this, share, s.E);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new WordSquare(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  SizeOptions opt("WordSquare");

  opt.solutions(1);
  opt.icl(ICL_BND);

  opt.parse(argc,argv);
  if (!opt.size()) {
    opt.size(3);
  }

  //
  // read the dictionary file and get all opt.size() sized words.
  // Here we use /usr/dict/words and only the lower case words
  //
  std::string line;
  std::ifstream dict;
  dict.open("/usr/share/dict/words");
  
  if (dict.is_open()) {
    while (!dict.eof()) {
      getline (dict, line);

      // accept only words with [a-z] (lower case)
      bool is_real_word = true;
      if (line.size() == opt.size()) {
        for(unsigned int i = 0; i < opt.size(); i++) {
          if (!isalpha(line[i]) || !islower(line[i])) {
            is_real_word = false;
            break;
          }
        }

        if (is_real_word) {
          _words.push_back(line);
        }
      }
    }
    dict.close();

  }  else {

    std::cout << "Unable to open file" << std::endl; 

  }

  num_words = _words.size();

  Script::run<WordSquare,DFS,SizeOptions>(opt);    

  return 0;

}
