%
% Puzzle in Minizinc

% From
%% http://groups.google.com/groups?q=FINITE+DOMAINS+With+Logic+Puzzles&hl=en&lr=&ie=UTF-8&c2coff=1&safe=off&selm=1992Jul27.034607.19386%40IRO.UMontreal.CA&rnum=4
% """"
% MUSIC MEN
%
% Three friends like different kinds of music.  From the clues given
% below, can you identify them, say how old each is, and work out
% his musical preference?
%
% Clues: 
% 1.      Rob is older than Queen, who likes classical music.
% 2.      The pop-music fan, who is not Prince, is not 24.
% 3.      Leon, who is not King, is 25.
% 4.      Mark's musical preference is not jazz.

% Knowledge: "this is what we know of the world."
% Names           : Leon, Mark, Rob.
% Surnames        : King, Prince, Queen.
% Ages            : 24, 25, 26.
% Music           : Classical, Jazz, Pop.
%
% """
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


include "globals.mzn";

set of int: persons = 1..3;
set of int: ages = 24..26;

var int: Age24; % = 24;
var int: Age25; % = 25;
var int: Age26; % = 26;
var ages: Leon;
var ages: Mark;
var ages: Rob;
var ages: King;
var ages: Prince;
var ages: Queen;
var ages: Classical;
var ages: Jazz;
var ages: Pop;

array[persons] of var ages: Age  = [Age24, Age25, Age26];
array[persons] of var ages: Names  = [King, Prince, Queen];
array[persons] of var ages: Surnames = [Leon, Mark, Rob];
array[persons] of var ages: Music    = [Classical, Jazz, Pop];

solve satisfy;

constraint

  all_different(Age) /\ 
  all_different(Names) /\ 
  all_different(Surnames) /\ 
  all_different(Music) /\ 

  Age24 = 24 /\ 
  Age25 = 25 /\
  Age26 = 26 /\

   % Rob is older than Queen /\  who likes classical music.
   Rob > Queen /\ 
   Queen = Classical /\ 

        % The pop-music fan /\  who is not Prince /\  is not 24.
        Pop != Prince /\ 
        Pop != Age24 /\ 

        % Leon /\  who is not King /\  is 25.
        Leon != King /\ 
        Leon = Age25 /\ 

        %  Mark's musical preference is not jazz.
        Mark != Jazz
;

output [
    "Age: ", show(Age), "\n",
    "Names: ", show(Names), "\n",
    "Surnames: ", show(Surnames), "\n",
    "Music: ", show(Music), "\n"
    
];
