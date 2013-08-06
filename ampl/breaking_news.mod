/*

  Breaking News puzzle (Dell Logic Puzzles) in AMPL+CP.
  
  Problem from  
  http://brownbuffalo.sourceforge.net/BreakingNewsClues.html
  """
  Title: Breaking News
  Author: Faith Johnson
  Publication: Dell Logic Puzzles
  Issue: April, 1998
  Page: 9
  Stars: 1

  The Daily Galaxy sent its four best reporters (Corey, Jimmy, Lois, and 
  Perry) to different locations (Bayonne, New Hope, Port Charles, and 
  South Amboy) to cover four breaking news events (30-pound baby, blimp 
  launching, skyscraper dedication, and beached whale). Their editor is 
  trying to remember where each of the reporters is. Can you match the name 
  of each reporter with the place he or she was sent, and the event that 
  each covered?
  
  1. The 30-pound baby wasn't born in South Amboy or New Hope.
  2. Jimmy didn't go to Port Charles.
  3. The blimp launching and the skyscraper dedication were covered, in some 
     order, by Lois and the reporter who was sent to Port Charles.
  4. South Amboy was not the site of either the beached whale or the 
     skyscraper dedication.
  5. Bayonne is either the place that Corey went or the place where the 
     whale was beached, or both.
  
  Determine: Reporter -- Location -- Story
  """
  
  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;

set Reporters;
set Locations;
set Events;

var reporters{Reporters} >= 1 <= n integer;
var locations{Locations} >= 1 <= n integer;
var events{Events} >= 1 <= n integer;

#
# constraints
#
s.t. c0a: alldiff{i in Reporters} reporters[i];
s.t. c0b: alldiff{i in Locations} locations[i];
s.t. c0c: alldiff{i in Events} events[i];

# Fix the reporters
s.t. c0d:
    reporters['Corey'] = 1 and
    reporters['Jimmy'] = 2 and
    reporters['Lois']  = 3 and
    reporters['Perry'] = 4
;

# 1. The 30-pound baby wasn't born in South Amboy or New Hope.
s.t. c1: 
   events['baby'] != locations['South_Amboy'] and
   events['baby'] != locations['New_Hope']
;
 

#  2. Jimmy didn't go to Port Charles.
s.t. c2:
   reporters['Jimmy'] != locations['Port_Charles']
;

# 3. The blimp launching and the skyscraper dedication were covered, 
#     in some order, by Lois and the reporter who was sent to 
#     Port Charles.
s.t. c3:
   (
     (events['blimp'] = reporters['Lois'] and events['skyscraper'] = locations['Port_Charles'])
     or
     (events['skyscraper'] = reporters['Lois'] and events['blimp'] = locations['Port_Charles'])
   )
;

#  4. South Amboy was not the site of either the beached whale or the 
#     skyscraper dedication.
s.t. c4:
   locations['South_Amboy'] != events['whale'] and
   locations['South_Amboy'] != events['skyscraper']
;

# 5. Bayonne is either the place that Corey went or the place where 
# the whale was beached, or both.
s.t. c5:
   (if locations['Bayonne'] = reporters['Corey'] then 1) + 
   (if locations['Bayonne'] = events['whale'] then 1)      >= 1
;


data;

param n := 4;

set Reporters = Corey Jimmy Lois Perry;
set Locations = Bayonne New_Hope Port_Charles South_Amboy;
set Events = baby blimp skyscraper whale;


option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display reporters, locations, events;

for{i in 1..n} {
   for{j in Reporters} { if reporters[j] = i then printf "%s was is ", j };
   for{j in Locations} { if locations[j] = i then printf "%s", j };
   for{j in Events} { if events[j] = i then printf " with the %s story.", j };
   printf "\n";
}