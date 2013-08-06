% 
% Tunalalooza puzzle (Dell Logic Puzzles) in MiniZinc.
% 
% http://brownbuffalo.sourceforge.net/TunapaloozaClues.html
% """
% Title: Tunapalooza
% Author: Eliot George
% Publication: Dell Logic Puzzles
% Issue: April, 1998
% Page: 10
% Stars: 2
%
% Tim and Keri have a full day ahead for themselves as they plan to see 
% and hear everything at Tunapalooza '98, the annual save-the-tuna benefit 
% concert in their hometown. To cover the most ground, they will have to 
% split up. They have arranged to meet during four rock band acts 
% (Ellyfish, Korrupt, Retread Ed and the Flat Tires, and Yellow Reef) at 
% planned rendezvous points (carnival games, information booth, mosh pit, 
% or T-shirt vendor). Can you help match each band name with the type of 
% music they play (country, grunge, reggae, or speed metal) and Tim and 
% Kerri's prearranged meeting spot while they play?
% 
% 1. Korrupt isn't a country or grunge music band.
% 2. Tim and Kerri won't meet at the carnival games during Ellyfish's 
%    performance.
% 3. The pair won't meet at the T-shirt vendor during the reggae band's show.
% 4. Exactly two of the following three statements are true:
% a) Ellyfish plays grunge music.
% b) Tim and Kerri won't meet at the information booth during a 
%    performance by Retread Ed and the Flat Tires.
% c) The two friends won't meet at the T-shirt vendor while Yellow Reef is playing.
% 5. The country and speed metal acts are, in some order, Retread Ed 
%    and the Flat Tires and the act during which Tim and Kerri will 
%    meet at the mosh pit.
% 6. The reggae band is neither Korrupt nor the act during which Tim and 
%    Kerri will meet at the information booth.
% 
% Determine: Band name -- Music type -- Meeting place
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% Solution
%  Ellyfish        Korrupt      Retread_Ed_and_the_Flat_Tires    Yellow_Reef
%  grunge          speed_metal  country                          reggae
%  T_shirt_vendor  mosh_pit     information_booth                carnival_games
% rockband: [1, 2, 3, 4]
% genre: [3, 1, 4, 2]
% rendevouz: [4, 3, 2, 1]


include "globals.mzn"; 
int: n = 4;
set of int: r = 1..n;

r: Ellyfish = 1;
r: Korrupt = 2;
r: Retread_Ed_and_the_Flat_Tires = 3;
r: Yellow_Reef =4;
array[r] of r: rock_band = [Ellyfish, Korrupt, Retread_Ed_and_the_Flat_Tires, Yellow_Reef];

var r: country;
var r: grunge;
var r: reggae;
var r: speed_metal;
array[r] of var r: genre = [country, grunge, reggae, speed_metal];

var r: carnival_games;
var r: information_booth;
var r: mosh_pit;
var r: T_shirt_vendor;
array[r] of var r: rendevouz = [carnival_games, information_booth, mosh_pit, T_shirt_vendor];


solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint
    all_different(genre)
    /\
    all_different(rendevouz)


    /\ % 1. Korrupt isn't a country or grunge music band.
    Korrupt != country /\ Korrupt != grunge

    /\ % 2. Tim and Kerri won't meet at the carnival games during Ellyfish's 
       %    performance.
    Ellyfish != carnival_games


    /\ % 3. The pair won't meet at the T-shirt vendor during the reggae 
       %    band's show.
    reggae != T_shirt_vendor

    /\ % 4. Exactly two of the following three statements are true:
       % a) Ellyfish plays grunge music.
       % b) Tim and Kerri won't meet at the information booth during a 
       %    performance by Retread Ed and the Flat Tires.
       % c) The two friends won't meet at the T-shirt vendor while 
       %    Yellow Reef is playing.
    (
       bool2int(Ellyfish = grunge) 
       +
       bool2int(information_booth != Retread_Ed_and_the_Flat_Tires)
       + 
       bool2int(T_shirt_vendor != Yellow_Reef)
    ) = 2


    /\ % 5. The country and speed metal acts are, in some order, Retread Ed 
       %    and the Flat Tires and the act during which Tim and Kerri will 
       %    meet at the mosh pit.
    (  
       ( country = Retread_Ed_and_the_Flat_Tires /\ speed_metal = mosh_pit )
       \/
       ( speed_metal = Retread_Ed_and_the_Flat_Tires /\ country = mosh_pit )
    )
       

    /\ % 6. The reggae band is neither Korrupt nor the act during which Tim and 
       %    Kerri will meet at the information booth.
    reggae != Korrupt /\
    reggae != information_booth
     

;

output [
  "rockband: ", show(rock_band), "\n",
  "genre: ", show(genre), "\n",
  "rendevouz: ", show(rendevouz), "\n",
];