/*

  Arch Friends puzzle (Dells Logic Puzzles) in SWI Prolog

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

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
    N = 4,
    Shoes = [_EcruEspadrilles, FuchsiaFlats, PurplePumps,
             SuedeSandals],
    Shoes ins 1..N,

    Store = [FootFarm, HeelsInAHandcart, TheShoePalace, Tootsies],
    Store ins 1..N,

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
    % sandals.
    TheShoePalace + 2 #= SuedeSandals,

    flatten([Shoes,Store],Vars),

    label(Vars),
    writeln(shoes=Shoes),
    writeln(store=Store),
    nl.
