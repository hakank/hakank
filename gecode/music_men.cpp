/*
  
  Music men puzzle in Gecode.

  """
  Three friends like different kinds of music.  From the clues given
  below, can you identify them, say how old each is, and work out
  his musical preference?

  Clues: 
  1.      Rob is older than Queen, who likes classical music.
  2.      The pop-music fan, who is not Prince, is not 24.
  3.      Leon, who is not King, is 25.
  4.      Mark's musical preference is not jazz.
  """

  Knowledge: "this is what we know of the world."
  Names           : Leon, Mark, Rob.
  Surnames        : King, Prince, Queen.
  Ages            : 24, 25, 26.
  Music           : Classical, Jazz, Pop.


  Solution:
    Leon Prince, 25, jazz.
    Mark Queen, 24, classical.
    Rob King, 26, pop.


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/music_men.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/music_men.pl
  * ECLiPSe: http://www.hakank.org/eclipse/music_men.ecl

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


class MusicMen : public Script {
protected:

  const static int n = 3;

  IntVarArray Names;
  IntVarArray Surnames;
  IntVarArray Music;

public:

  MusicMen(const Options& opt) 
    : 
    Names(*this, n, 24, 26),
    Surnames(*this, n, 24, 26),
    Music(*this, n, 24, 26)
  {

    int Age24 = 24;
    int Age25 = 25;
    // int Age26 = 26;

    // Names
    IntVar
      King(Names[0]),
      Prince(Names[1]),
      Queen(Names[2]);

    // Surnames
    IntVar
      Leon(Surnames[0]),
      Mark(Surnames[1]),
      Rob(Surnames[2]);

    IntVar
      Classical(Music[0]),
      Jazz(Music[1]),
      Pop(Music[2]);

    distinct(*this, Names);
    distinct(*this, Surnames);
    distinct(*this, Music);


    // Rob is older than Queen, who likes classical music.
    rel(*this, Rob > Queen);
    rel(*this, Queen == Classical);

    // The pop-music fan, who is not Prince, is not 24.
    rel(*this, Pop != Prince);
    rel(*this, Pop != Age24);

    // Leon, who is not King, is 25.
    rel(*this, Leon != King);
    rel(*this, Leon == Age25);

    //  Mark's musical preference is not jazz.
    rel(*this, Mark != Jazz);


    // branching
    branch(*this, Names, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    branch(*this, Surnames, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    branch(*this, Music, INT_VAR_SIZE_MIN(), INT_VAL_MIN());

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "Names: " << Names << endl;
    os << "Surnames: " << Surnames << endl;
    os << "Music: " << Music << endl;
    os << endl;
  }


  // Constructor for cloning s
  MusicMen(bool share, MusicMen& s) : Script(share,s) {
    Names.update(*this, share, s.Names);
    Surnames.update(*this, share, s.Surnames);
    Music.update(*this, share, s.Music);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new MusicMen(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("MusicMen");

  opt.solutions(0);

  opt.parse(argc,argv);

  Script::run<MusicMen,DFS,Options>(opt);
    
  return 0;
}


