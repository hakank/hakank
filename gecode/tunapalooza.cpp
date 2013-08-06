/*

  Tunalalooza puzzle (Dell Logic Puzzles) in Gecode.

  http://brownbuffalo.sourceforge.net/TunapaloozaClues.html
  """
  Title: Tunapalooza
  Author: Eliot George
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 10
  Stars: 2
 
  Tim and Keri have a full day ahead for themselves as they plan to see 
  and hear everything at Tunapalooza '98, the annual save-the-tuna benefit 
  concert in their hometown. To cover the most ground, they will have to 
  split up. They have arranged to meet during four rock band acts 
  (Ellyfish, Korrupt, Retread Ed and the Flat Tires, and Yellow Reef) at 
  planned rendezvous points (carnival games, information booth, mosh pit, 
  or T-shirt vendor). Can you help match each band name with the type of 
  music they play (country, grunge, reggae, or speed metal) and Tim and 
  Kerri's prearranged meeting spot while they play?
  
  1. Korrupt isn't a country or grunge music band.
  2. Tim and Kerri won't meet at the carnival games during Ellyfish's 
     performance.
  3. The pair won't meet at the T-shirt vendor during the reggae band's show.
  4. Exactly two of the following three statements are true:
  a) Ellyfish plays grunge music.
  b) Tim and Kerri won't meet at the information booth during a 
     performance by Retread Ed and the Flat Tires.
  c) The two friends won't meet at the T-shirt vendor while Yellow Reef is playing.
  5. The country and speed metal acts are, in some order, Retread Ed 
     and the Flat Tires and the act during which Tim and Kerri will 
     meet at the mosh pit.
  6. The reggae band is neither Korrupt nor the act during which Tim and 
     Kerri will meet at the information booth.
  
  Determine: Band name -- Music type -- Meeting place
  """  

  Compare with my other models:
  * MiniZinc: http://www.hakank.org/minizinc/tunapalooza.mzn
  * ECLiPSe : http://www.hakank.org/eclipse/tunapalooza.ecl
  * SICStus Prolog: http://www.hakank.org/sicstus/tunapalooza.pl

  This Gecode model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also, see my Gecode page: http://www.hakank.org/gecode/ .

*/

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;


class Tunapalooza : public Script {
protected:

  static const int n = 4;

  IntVarArray genre;
  IntVarArray rendevouz;


public:

  Tunapalooza(const Options& opt) 
  : 
    genre(*this, n, 1, n),
    rendevouz(*this, n, 1, n)
  {

    enum {
      dummy, 
      Ellyfish,
      Korrupt,
      Retread_Ed_and_the_Flat_Tires,
      Yellow_Reef
    };

    IntVar
      country(genre[0]), 
      grunge(genre[1]), 
      reggae(genre[2]), 
      speed_metal(genre[3]);

    IntVar
      carnival_games(rendevouz[0]),
      information_booth(rendevouz[1]),
      mosh_pit(rendevouz[2]),
      T_shirt_vendor(rendevouz[3]);

    distinct(*this, genre, opt.icl());
    distinct(*this, rendevouz, opt.icl());

    // 1. Korrupt isn't a country or grunge music band.
    rel(*this, Korrupt != country && Korrupt != grunge);

    // 2. Tim and Kerri won't meet at the carnival games during Ellyfish's 
    //    performance.
    rel(*this, Ellyfish != carnival_games);


    // 3. The pair won't meet at the T-shirt vendor during the reggae 
    //    band's show.
    rel(*this, reggae != T_shirt_vendor);

    // 4. Exactly two of the following three statements are true:
    // a) Ellyfish plays grunge music.
    // b) Tim and Kerri won't meet at the information booth during a 
    //    performance by Retread Ed and the Flat Tires.
    // c) The two friends won't meet at the T-shirt vendor while 
    //    Yellow Reef is playing.
    rel(*this, 
        (
         expr(*this, Ellyfish == grunge) 
         +
         expr(*this, information_booth != Retread_Ed_and_the_Flat_Tires)
         + 
         expr(*this, T_shirt_vendor != Yellow_Reef)
         ) == 2);

    // 5. The country and speed metal acts are, in some order, Retread Ed 
    //    and the Flat Tires and the act during which Tim and Kerri will 
    //    meet at the mosh pit.
    rel(*this,  
        ( country == Retread_Ed_and_the_Flat_Tires && speed_metal == mosh_pit )
        ||
        ( speed_metal == Retread_Ed_and_the_Flat_Tires && country == mosh_pit )
        );
       

    // 6. The reggae band is neither Korrupt nor the act during which Tim and 
    //    Kerri will meet at the information booth.
    rel(*this, 
        reggae != Korrupt &&
        reggae != information_booth
        );
     
 
    branch(*this, genre, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 
    branch(*this, rendevouz, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); 

  }

  // Print solution
  virtual void
  print(std::ostream& os) const {
    os << "genre    : " << genre << std::endl;
    os << "rendevouz: " << rendevouz << std::endl;
    os << std::endl;
  }

  // Constructor for cloning s
  Tunapalooza(bool share, Tunapalooza& s) : Script(share,s) {
    genre.update(*this, share, s.genre);
    rendevouz.update(*this, share, s.rendevouz);
  }

  // Copy during cloning
  virtual Space*
  copy(bool share) {
    return new Tunapalooza(share,*this);
  }
};


int
main(int argc, char* argv[]) {

  Options opt("Tunapalooza");

  opt.solutions(0);
  opt.parse(argc,argv);

  Script::run<Tunapalooza,DFS,Options>(opt);

  return 0;
}


