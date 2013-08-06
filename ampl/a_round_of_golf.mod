/*

  A round of golf puzzle (Dell Logic Puzzles) in AMPL.

  From http://brownbuffalo.sourceforge.net/RoundOfGolfClues.html
  """
  Title: A Round of Golf
  Author: Ellen K. Rodehorst
  Publication: Dell Favorite Logic Problems
  Issue: Summer, 2000
  Puzzle #: 9
  Stars: 1

  When the Sunny Hills Country Club golf course isn't in use by club members, 
  of course, it's open to the club's employees. Recently, Jack and three other 
  workers at the golf course got together on their day off to play a round of 
  eighteen holes of golf. 
  Afterward, all four, including Mr. Green, went to the clubhouse to total 
  their scorecards. Each man works at a different job (one is a short-order 
  cook), and each shot a different score in the game. No one scored below 
  70 or above 85 strokes. From the clues below, can you discover each man's 
  full name, job and golf score?
  
  1. Bill, who is not the maintenance man, plays golf often and had the lowest 
  score of the foursome.
  2. Mr. Clubb, who isn't Paul, hit several balls into the woods and scored ten 
  strokes more than the pro-shop clerk.
  3. In some order, Frank and the caddy scored four and seven more strokes than 
  Mr. Sands.
  4. Mr. Carter thought his score of 78 was one of his better games, even 
     though Frank's score  was lower.
  5. None of the four scored exactly 81 strokes.
  
  Determine: First Name - Last Name - Job - Score 
  """

  Compare with the F1 model: 
  http://www.f1compiler.com/samples/A%20Round%20of%20Golf.f1.html

  Solution:
             Jack, Bill, Paul, Frank
             Clubb Sands Carter Green
             maint cook  caddy clerk
             85    71    78    75

  first_name: [1, 2, 3, 4]
  last_name : [4, 1, 2, 3]
  job       : [2, 1, 4, 3]
  score     : [85, 71, 78, 75]

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;
param Jack;
param Bill;
param Paul;
param Frank;


set first_names;
set last_names;
set jobs;

var last_name{last_names} >= 1 <= 4 integer;
var job{jobs} >= 1 <= 4 integer;
var score{1..n} >= 70 <= 85 integer;

# To simplify the output (they are assigned to constants later).
var first_name{first_names} >= 1 <= 4 integer;

#
# constraints
#
s.t. c0a: alldiff{i in last_names} last_name[i];
s.t. c0b: alldiff{i in jobs} job[i];
s.t. c0c: alldiff{i in 1..n} score[i];

# 1. Bill, who is not the maintenance man, plays golf often and had 
#    the lowest score of the foursome.
s.t. c1: Bill != job['maintenance_man'] and
         score[Bill] < score[Jack] and
         score[Bill] < score[Paul] and
         score[Bill] < score[Frank]
;

# 2. Mr. Clubb, who isn't Paul, hit several balls into the woods and 
#    scored ten strokes more than the pro-shop clerk.
# Note: fake element constraint with exists{...}.
s.t. c2: last_name['Clubb'] != Paul and
         exists{clubb in 1..n, clerk in 1..n}
            clubb = last_name['Clubb'] and 
            clerk = job['clerk'] and
            score[clubb] = score[clerk] + 10
;


# 3. In some order, Frank and the caddy scored four and seven more 
#    strokes than Mr. Sands.
s.t. c3: Frank != job['caddy'] and
         Frank != last_name['Sands'] and
         job['caddy'] != last_name['Sands']
;

# Note: fake element constraint with exists{...}.
s.t. c3b:  exists{sands in 1..n, caddy in 1..n}
              sands = last_name['Sands'] and
              caddy = job['caddy'] and
              (
                (score[Frank] = score[sands] + 4 and
                 score[caddy] = score[sands] + 7 )
                or
                (score[Frank] = score[sands] + 7 and
                 score[caddy] = score[sands] + 4 )
              )
 ;

# 4. Mr. Carter thought his score of 78 was one of his better games, even 
#    though Frank's score was lower.
# Note: fake element constraint with exists{...}.
s.t. c4:  Frank != last_name['Carter'] and
          exists{carter in 1..n}
             carter = last_name['Carter'] and
             score[carter] = 78 and
             score[Frank] < score[carter]
;

#  5. None of the four scored exactly 81 strokes.
s.t. c5{i in 1..n}:  score[i] != 81;

# And assign the first names.
s.t. c6:  first_name['Jack'] = Jack and
          first_name['Bill'] = Bill and
          first_name['Paul'] = Paul and
          first_name['Frank'] = Frank
;

data;

param n := 4;
param Jack := 1;
param Bill := 2;
param Paul := 3;
param Frank := 4;

set first_names = "Jack", "Bill", "Paul", "Frank";

# set first_names = Jack Bill Paul Frank;
set last_names = Green Clubb Sands Carter;
set jobs = cook maintenance_man clerk caddy;


option show_stats 2;

option solver gecode;
option gecode_options "icl=def var_branching=size_min val_branching=min outlev=1 outfreq=1 timelimit=30";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";


solve;
# write gtest;

# display belongs;
display Jack, Bill, Paul, Frank,last_name,job,score;

for{i in 1..n} {
  for{j in first_names} { if first_name[j] = i then printf "%-8s ", j };
  for{j in last_names} { if last_name[j] = i then printf "%-10s ", j };
  for{j in jobs} { if job[j] = i then { printf "%-16s ", j} };
  printf "%-14s ", score[i];
  printf "\n";

}