/*

  Lecture Series Puzzle (Dell Logic Puzzles) in AMPL+CP.

  Problem from
  http://brownbuffalo.sourceforge.net/LectureSeriesClues.html
  """
  Title: Lecture Series
  Author: Alex Knight
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 10
  Stars: 2

  Last week at school was made varied by a series of lectures, one each 
  day (Monday through Friday), in the auditorium. None of the lectures was 
  particularly interesting (on choosing a college, physical hygiene, modern 
  art, nutrition, and study habits), but the students figured that anything 
  that got them out of fourth period was okay. The lecturers were two women 
  named Alice and Bernadette, and three men named Charles, Duane, and Eddie; 
  last names were Felicidad, Garber, Haller, Itakura, and Jeffreys. 
  Can you find each day's lecturer and subject?

  1. Alice lectured on Monday.
  2. Charles's lecture on physical hygiene wasn't given on Friday.
  3. Dietician Jeffreys gave the lecture on nutrition.
  4. A man gave the lecture on modern art.
  5. Ms. Itakura and the lecturer on proper study habits spoke on 
     consecutive days, in one order or the other.
  6. Haller gave a lecture sometime after Eddie did.
  7. Duane Felicidad gave his lecture sometime before the modern art lecture. 
  """

            Monday           Tuesday         Wednesday          Thursday            Friday
             Alice             Duane             Eddie           Charles        Bernadette
           Itakura         Felicidad            Garber            Haller          Jeffreys
  Choosing College      Study Habits        Modern Art  Physical Hygiene         Nutrition



  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;

param Monday := 1;
param Tuesday := 2;
param Wednesday := 3;
param Thursday := 4;
param Friday := 5;

var choosing_a_college >= 1 <= n integer;
var physical_hygiene >= 1 <= n integer;
var modern_art >= 1 <= n integer;
var nutrition >= 1 <= n integer;
var study_habits >= 1 <= n integer;
var lectures{1..n} >= 1 <= n integer;

var Alice >= 1 <= n integer;
var Bernadette >= 1 <= n integer;
var Charles >= 1 <= n integer;
var Duane >= 1 <= n integer;
var Eddie >= 1 <= n integer;
var first_names{1..n} >= 1 <= n integer;

var Felicidad >= 1 <= n integer;
var Garber >= 1 <= n integer;
var Haller >= 1 <= n integer;
var Itakura >= 1 <= n integer;
var Jeffreys >= 1 <= n integer;
var last_names{1..n} >= 1 <= n integer;


#
# constraints
#
s.t. t1: lectures[1] = choosing_a_college and
         lectures[2] = physical_hygiene and
         lectures[3] = modern_art and 
         lectures[4] = nutrition and 
         lectures[5] = study_habits;
s.t. t2: 
         first_names[1] = Alice and
         first_names[2] = Bernadette and
         first_names[3] = Charles and
         first_names[4] = Duane and
         first_names[5] = Eddie
;

s.t. t3:
         last_names[1] = Felicidad and
         last_names[2] = Garber and
         last_names[3] = Haller and
         last_names[4] = Itakura and
         last_names[5] = Jeffreys
;

s.t. c0a: alldiff{i in 1..n} lectures[i];
s.t. c0b: alldiff{i in 1..n} first_names[i];
s.t. c0c: alldiff{i in 1..n} last_names[i];


# 1. Alice lectured on Monday.
s.t. c1: Alice = Monday;

# 2. Charles's lecture on physical hygiene wasn't given on Friday.
s.t. c2: Charles = physical_hygiene and
         Charles != Friday and
         physical_hygiene != Friday 
;

# 3. Dietician Jeffreys gave the lecture on nutrition.
s.t. c3: Jeffreys = nutrition;

# 4. A man gave the lecture on modern art.
s.t. c4: 
    modern_art = Charles or
    modern_art = Duane or
    modern_art = Eddie
;

# 5. Ms. Itakura and the lecturer on proper study habits spoke on 
#    consecutive days, in one order or the other.
s.t. c5: (
          Itakura = Alice or Itakura = Bernadette
         ) 
         and
         (
          abs(Itakura - study_habits) = 1
         )
         and 
         (Itakura != study_habits)
;

# 6. Haller gave a lecture sometime after Eddie did.
s.t. c6: Haller > Eddie;

# 7. Duane Felicidad gave his lecture sometime before the modern art lecture. 
s.t. c7: Duane = Felicidad and
         Duane < modern_art
;



data;

param n := 5;


# option presolve 0;
option show_stats 2;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display Monday, Tuesday,Wednesday,Thursday,Friday;
display choosing_a_college, physical_hygiene, modern_art, nutrition, study_habits;
display Alice, Bernadette, Charles, Duane, Eddie;
display Felicidad, Garber, Haller, Itakura,Jeffreys;

display lectures, first_names, last_names;
