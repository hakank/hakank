% 
% Arch friends puzzle (Dell Logic Puzzles) in MiniZinc.
% 
% Problem formulation from 
% http://brownbuffalo.sourceforge.net/ArchFriendsClues.html
% """
% Title: Arch Friends
% Author: Mark T. Zegarelli
% Publication: Dell Logic Puzzles
% Issue: April, 1998
% Page: 7
% Stars: 1
%
% Harriet, upon returning from the mall, is happily describing her four shoe 
% purchases to her friend Aurora. Aurora just loves the four different kinds 
% of shoes that Harriet bought (ecru espadrilles, fuchsia flats, purple pumps, 
% and suede sandals), but Harriet can't recall at which different store (Foot 
% Farm, Heels in a Handcart, The Shoe Palace, or Tootsies) she got each pair. 
% Can you help these two figure out the order in which Harriet bought each 
% pair of shoes, and where she bought each?
%
% 1. Harriet bought fuchsia flats at Heels in a Handcart.
% 2. The store she visited just after buying her purple pumps was not Tootsies.
% 3. The Foot Farm was Harriet's second stop.
% 4. Two stops after leaving The Shoe Place, Harriet bought her suede sandals.
% 
% Determine: Order - Shoes - Store 
% """

% Compare with the F1 model:
% http://www.f1compiler.com/samples/Arch%20Friends.f1.html


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 4;
set of int: r = 1..4;

var r: ecru_espadrilles;
var r: fuchsia_flats;
var r: purple_pumps;
var r: suede_sandals;

var r: Foot_Farm;
var r: Heels_in_a_Handcart;
var r: The_Shoe_Palace;
var r: Tootsies;

array[1..n] of var r: shoes = [ecru_espadrilles, fuchsia_flats, purple_pumps, suede_sandals];
array[1..n] of var r: shops = [Foot_Farm, Heels_in_a_Handcart, The_Shoe_Palace, Tootsies];



solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint
  all_different(shoes)
  /\
  all_different(shops)

  /\ % 1. Harriet bought fuchsia flats at Heels in a Handcart.
  fuchsia_flats = Heels_in_a_Handcart

  /\  %  2. The store she visited just after buying her purple pumps was not Tootsies.
  purple_pumps + 1 != Tootsies

  /\ %  3. The Foot Farm was Harriet's second stop.
  Foot_Farm = 2

  /\ %  4. Two stops after leaving The Shoe Place, Harriet bought her suede sandals.
  The_Shoe_Palace + 2 = suede_sandals

;

output [
  "shoes [ecru_espadrilles, fuchsia_flats, purple_pumps, suede_sandals] ", show(shoes),"\n",
  "shops [Foot_Farm, Heels_in_a_Handcart, The_Shoe_Palace, Tootsies]", show(shops),"\n",

];

