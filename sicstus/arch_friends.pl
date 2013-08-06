/*

  Arch Friends puzzle (Dell Logic Puzzles) in SICStus Prolog.

  From http://brownbuffalo.sourceforge.net/ArchFriendsClues.html
  """
  Title: Arch Friends
  Author: Mark T. Zegarelli
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 7
  Stars: 1

  Harriet, upon returning from the mall, is happily describing her four
  shoe purchases to her friend Aurora. Aurora just loves the four
  different kinds of shoes that Harriet bought 
     (ecru espadrilles,fuchsia flats, purple pumps, and suede sandals),
  but Harriet can't recall at which different store 
     (Foot Farm, Heels in a Handcart, The Shoe Palace, or Tootsies) 
  she got each pair. Can you help these two figure out the order in
  which Harriet bought each pair of shoes, and where she bought each?

  1. Harriet bought fuchsia flats at Heels in a Handcart.
  2. The store she visited just after buying her purple pumps was not
     Tootsies.
  3. The Foot Farm was Harriet's second stop.
  4. Two stops after leaving The Shoe Place, Harriet bought her suede
     sandals.

  Determine: Order - Shoes - Store 
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/arch_friends.mzn
  * ECLiPSe : http://www.hakank.org/eclipse/arch_friends.ecl
  

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        N = 4,
        Shoes = [EcruEspadrilles, FuchsiaFlats, PurplePumps,
                 SuedeSandals],
        domain(Shoes, 1,N),

        Store = [FootFarm, HeelsInAHandcart, TheShoePalace, Tootsies],
        domain(Store, 1,N),

        all_different(Shoes),
        all_different(Store),

        % 1. Harriet bought fuchsia flats at Heels in a Handcart.
        FuchsiaFlats #= HeelsInAHandcart,

        % 2. The store she visited just after buying her purple pumps was not
        %    Tootsies.
        PurplePumps + 1 #\= Tootsies,

        % 3. The Foot Farm was Harriet's second stop.
        FootFarm #= 2,

        % 4. Two stops after leaving The Shoe Place, Harriet bought her suede
        %    sandals.
        TheShoePalace + 2 #= SuedeSandals,

        append(Shoes,Store, Vars),

        labeling([],Vars),
        write(shoes:Shoes),nl,
        write(store:Store),nl,
        nl,fail.
