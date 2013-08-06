% 
% Lecture Series Puzzle (Dell Logic Puzzles) in MiniZinc.
% 
% Problem from
% http://brownbuffalo.sourceforge.net/LectureSeriesClues.html
% """
% Title: Lecture Series
% Author: Alex Knight
% Publication: Dell Logic Puzzles
% Issue: April, 1998
% Page: 10
% Stars: 2
%
% Last week at school was made varied by a series of lectures, one each 
% day (Monday through Friday), in the auditorium. None of the lectures was 
% particularly interesting (on choosing a college, physical hygiene, modern 
% art, nutrition, and study habits), but the students figured that anything 
% that got them out of fourth period was okay. The lecturers were two women 
% named Alice and Bernadette, and three men named Charles, Duane, and Eddie; 
% last names were Felicidad, Garber, Haller, Itakura, and Jeffreys. 
% Can you find each day's lecturer and subject?
%
% 1. Alice lectured on Monday.
% 2. Charles's lecture on physical hygiene wasn't given on Friday.
% 3. Dietician Jeffreys gave the lecture on nutrition.
% 4. A man gave the lecture on modern art.
% 5. Ms. Itakura and the lecturer on proper study habits spoke on 
%    consecutive days, in one order or the other.
% 6. Haller gave a lecture sometime after Eddie did.
% 7. Duane Felicidad gave his lecture sometime before the modern art lecture. 
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% Solution:
% Monday              Tuesday          Wednesday  Thursday           Friday
% Alice               Duane            Eddie      Charles            Bernadette
% Itakura             Felicidad        Garber     Haller             Jeffreys
% choosing_a_college  study_habits     modern_art physical_hygiene   nutrition
%                
% days: [1, 2, 3, 4, 5]
% first_name: [1, 5, 4, 2, 3]
% last_name: [2, 3, 4, 1, 5]
% lectures: [1, 4, 3, 5, 2]


include "globals.mzn"; 
int: n = 5;
set of int: r = 1..n;

r: Monday = 1;
r: Tuesday = 2;
r: Wednesday = 3;
r: Thursday = 4;
r: Friday = 5;
array[r] of r: days = [Monday, Tuesday, Wednesday, Thursday, Friday];

var r: choosing_a_college;
var r: physical_hygiene;
var r: modern_art;
var r: nutrition;
var r: study_habits;
array[r] of var r: lectures = [choosing_a_college, physical_hygiene, modern_art, nutrition, study_habits];

var r: Alice;
var r: Bernadette;
var r: Charles;
var r: Duane;
var r: Eddie;
array[r] of var r: first_name = [Alice, Bernadette, Charles, Duane, Eddie];

var r: Felicidad;
var r: Garber;
var r: Haller;
var r: Itakura;
var r: Jeffreys;
array[r] of var r: last_name = [Felicidad, Garber, Haller, Itakura, Jeffreys];




solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint
   all_different(lectures) /\
   all_different(first_name) /\
   all_different(last_name) 

   /\ % 1. Alice lectured on Monday.
   Alice = Monday

   /\ % 2. Charles's lecture on physical hygiene wasn't given on Friday.
   Charles = physical_hygiene
   /\ Charles != Friday
   /\ physical_hygiene != Friday 

   /\ % 3. Dietician Jeffreys gave the lecture on nutrition.
   Jeffreys = nutrition 

   /\ % 4. A man gave the lecture on modern art.
   ( 
     modern_art = Charles \/ 
     modern_art = Duane \/ 
     modern_art = Eddie
   )

   /\ % 5. Ms. Itakura and the lecturer on proper study habits spoke on 
      %    consecutive days, in one order or the other.
   (
     Itakura = Alice \/ Itakura = Bernadette
   ) 
   /\
   (
     abs(Itakura - study_habits) = 1
   )
   /\ 
   (Itakura != study_habits)

   /\ % 6. Haller gave a lecture sometime after Eddie did.
   Haller > Eddie

   /\ % 7. Duane Felicidad gave his lecture sometime before the modern art lecture. 
   Duane = Felicidad /\
   Duane < modern_art


;

output [
  "days: ", show(days), "\n",
  "first_name: ", show(first_name), "\n",
  "last_name: ", show(last_name), "\n",
  "lectures: ", show(lectures), "\n", 
];