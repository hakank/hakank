% 
% Breaking News puzzle (Dell Logic Puzzles) in MiniZinc.
%
% Problem from  
% http://brownbuffalo.sourceforge.net/BreakingNewsClues.html
% """
% Title: Breaking News
% Author: Faith Johnson
% Publication: Dell Logic Puzzles
% Issue: April, 1998
% Page: 9
% Stars: 1
%
% The Daily Galaxy sent its four best reporters (Corey, Jimmy, Lois, and 
% Perry) to different locations (Bayonne, New Hope, Port Charles, and 
% South Amboy) to cover four breaking news events (30-pound baby, blimp 
% launching, skyscraper dedication, and beached whale). Their editor is 
% trying to remember where each of the reporters is. Can you match the name 
% of each reporter with the place he or she was sent, and the event that 
% each covered?
% 
% 1. The 30-pound baby wasn't born in South Amboy or New Hope.
% 2. Jimmy didn't go to Port Charles.
% 3. The blimp launching and the skyscraper dedication were covered, in some 
%    order, by Lois and the reporter who was sent to Port Charles.
% 4. South Amboy was not the site of either the beached whale or the 
%    skyscraper dedication.
% 5. Bayonne is either the place that Corey went or the place where the 
%    whale was beached, or both.
% 
% Determine: Reporter -- Location -- Story
% """

%
% Compare with the F1 model:
% http://www.f1compiler.com/samples/Breaking%20News.f1.html

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% Solution:
%             Corey    Jimmy     Lois        Perry
%             Bayonne  New Hope  South Amboy Port Charles
%             baby     whale     blimp       skyscraper 
% locations = [1, 2, 4, 3]
% events = [1, 3, 4, 2]


include "globals.mzn"; 
int: n = 4;
set of int: r = 1..n;
r: Corey = 1;
r: Jimmy = 2;
r: Lois = 3;
r: Perry = 4;
array[r] of r:  reporters = [Corey, Jimmy, Lois, Perry];

var r: Bayonne;
var r: New_Hope;
var r: Port_Charles;
var r: South_Amboy;
array[r] of var r: locations = [Bayonne, New_Hope, Port_Charles, South_Amboy];

var r: baby;
var r: blimp;
var r: skyscraper;
var r: whale;
array[r] of var r: events = [baby, blimp, skyscraper, whale];

solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint
   all_different(locations) 
   /\
   all_different(events)

   /\ % 1. The 30-pound baby wasn't born in South Amboy or New Hope.
   baby != South_Amboy /\
   baby != New_Hope
 
   /\ %  2. Jimmy didn't go to Port Charles.
   Jimmy != Port_Charles

   /\ %  3. The blimp launching and the skyscraper dedication were covered, 
      %     in some order, by Lois and the reporter who was sent to 
      %     Port Charles.
   (
     (blimp = Lois /\ skyscraper = Port_Charles)
     \/
     (skyscraper = Lois /\ blimp = Port_Charles)
   )

   /\ %  4. South Amboy was not the site of either the beached whale or the 
      %     skyscraper  dedication.
   South_Amboy != whale /\
   South_Amboy != skyscraper

   /\ %  5. Bayonne is either the place that Corey went or the place where 
      % the whale was beached, or both.
   ( bool2int(Bayonne = Corey) + bool2int(Bayonne = whale)  >= 1)

;

output [
  "reporters: ", show(reporters), "\n",
  "locations: ", show(locations), "\n",
  "events: ", show(events), "\n",

];
