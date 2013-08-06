% 
% Eliza Pseudonym of Puzzlania 7 in MiniZinc.
% 
% From 
% A Cleverly-Titled Logic Puzzle Blog (or Or, A Logically-Titled Clever Puzzle Blog)
% http://mathgrant.blogspot.com/2008/10/puzzle-94-eliza-pseudonym-of-puzzlania.html
% 
% """
% During a six-day period from Monday through Saturday, Eliza Pseudonym and 
% her friends Anna, Barbra, Carla, Delilah, and Fiona have subscribed to an 
% internet mailing list that features a new word every day. No two women 
% subscribed on the same day. On each day during the six-day period, a 
% different word has been featured (abulia, betise, caryatid, dehisce, 
% euhemerism, and floruit, in some order). From the clues below, determine 
% the day on which each woman subscribed, and the day on which each word 
% was featured.
% 
% 1. Exactly one of the women has a name beginning with the same letter of 
% the alphabet as the word featured on the day that she subscribed to the 
% mailing list.
% 2. The word "caryatid" was featured precisely two days prior to Fiona 
% joining the mailing list.
% 3. Carla joined the mailing list on Friday.
% 4. Anna signed up for the mailing list precisely one day after the word 
% "euhemerism" was highlighted.
% 5. Wednesday's word did not end with the letter "e".
% 6. Barbra subscribed precisely three days after the word "dehisce" was 
% featured. 
% """
% 

% Solution:
% friends  : [3, 4, 5, 2, 1, 6]
% all_words: [3, 6, 4, 1, 2, 5]
% anna   : 3
% barbra : 4
% carla  : 5
% delilah: 2
% eliza  : 1
% fiona  : 6
% abulia    : 3
% betise    : 6
% caryatid  : 4
% dehisce   : 1
% euhemerism: 2
% floruit   : 5
%
% I.e.
%
% Day        Who     Word        
% monday   : eliza   dehisce
% tuesday  : delilah euhemerism
% wednesday: anna    abulia
% thursday : barbra  caryatid
% friday   : carla   floruit
% saturday : fiona   betise
%



% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

set of int: days = 1..6;
days: monday = 1;
days: tuesday = 2;
days: wednesday = 3;
days: thursday = 4;
days: friday = 5;
days: saturday = 6;

var days: anna;
var days: barbra;
var days: carla;
var days: delilah;
var days: eliza;
var days: fiona;
array[days] of var days: friends = [anna, barbra, carla, delilah, eliza, fiona];

var days: abulia;
var days: betise;
var days: caryatid;
var days: dehisce;
var days: euhemerism;
var days: floruit;
array[days] of var days: all_words = [abulia,betise, caryatid,dehisce,euhemerism,floruit];

var days: sum_beginnings = bool2int(anna     = abulia)     +
                           bool2int(barbra   = betise)     +
                           bool2int(caryatid = carla)      +
                           bool2int(dehisce  = delilah)    +
                           bool2int(eliza    = euhemerism) +
                           bool2int(fiona    = floruit)    ;

% solve satisfy;
solve :: int_search(friends ++ all_words ++ [sum_beginnings], first_fail, indomain_min, complete) satisfy;

constraint

  all_different(friends)
  /\
  all_different(all_words)


  % 1. Exactly one of the women has a name beginning with the same letter of 
  % the alphabet as the word featured on the day that she subscribed to the 
  % mailing list.
  /\
  1 = sum_beginnings

  % 2. The word "caryatid" was featured precisely two days prior to Fiona 
  % joining the mailing list.
  /\
  caryatid = fiona - 2

  % 3. Carla joined the mailing list on Friday.
  /\
  carla = friday

  % 4. Anna signed up for the mailing list precisely one day after the word 
  % "euhemerism" was highlighted.
  /\
  anna = euhemerism + 1 

  % 5. Wednesday's word did not end with the letter "e".
  /\
  (
  wednesday = abulia     \/
  wednesday = caryatid   \/
  wednesday = euhemerism \/
  wednesday = floruit
  )

  % 6. Barbra subscribed precisely three days after the word "dehisce" was 
  % featured. 
  /\
  barbra = dehisce + 3

;

output [
  "friends  : ", show(friends), "\n",
  "all_words: ", show(all_words), "\n", 

  "anna   : ", show(anna), "\n",
  "barbra : ", show(barbra), "\n",
  "carla  : ", show(carla), "\n",
  "delilah: ", show(delilah), "\n",
  "eliza  : ", show(eliza), "\n",
  "fiona  : ", show(fiona), "\n",

  "abulia    : ", show(abulia), "\n",
  "betise    : ", show(betise), "\n",
  "caryatid  : ", show(caryatid), "\n",
  "dehisce   : ", show(dehisce), "\n",
  "euhemerism: ", show(euhemerism), "\n",
  "floruit   : ", show(floruit), "\n",


];