% 
% A Round of Golf puzzle (Dell Logic Puzzles) in MiniZinc.
% 
% From http://brownbuffalo.sourceforge.net/RoundOfGolfClues.html
% """
% Title: A Round of Golf
% Author: Ellen K. Rodehorst
% Publication: Dell Favorite Logic Problems
% Issue: Summer, 2000
% Puzzle #: 9
% Stars: 1
%
% When the Sunny Hills Country Club golf course isn't in use by club members, 
% of course, it's open to the club's employees. Recently, Jack and three other 
% workers at the golf course got together on their day off to play a round of 
% eighteen holes of golf. 
% Afterward, all four, including Mr. Green, went to the clubhouse to total 
% their scorecards. Each man works at a different job (one is a short-order 
% cook), and each shot a different score in the game. No one scored below 
% 70 or above 85 strokes. From the clues below, can you discover each man's 
% full name, job and golf score?
% 
% 1. Bill, who is not the maintenance man, plays golf often and had the lowest 
% score of the foursome.
% 2. Mr. Clubb, who isn't Paul, hit several balls into the woods and scored ten 
% strokes more than the pro-shop clerk.
% 3. In some order, Frank and the caddy scored four and seven more strokes than 
% Mr. Sands.
% 4. Mr. Carter thought his score of 78 was one of his better games, even 
%    though Frank's score  was lower.
% 5. None of the four scored exactly 81 strokes.
% 
% Determine: First Name - Last Name - Job - Score 
% """

% Compare with the F1 model: 
% http://www.f1compiler.com/samples/A%20Round%20of%20Golf.f1.html

% Solution:
%            Jack, Bill, Paul, Frank
%            Clubb Sands Carter Green
%            maint cook  caddy clerk
%            85    71    78    75
% first_name: [1, 2, 3, 4]
% last_name : [4, 1, 2, 3]
% job       : [2, 1, 4, 3]
% score     : [85, 71, 78, 75]



% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 4;

set of int: r = 1..n;
r: Jack = 1;
r: Bill = 2;
r: Paul = 3;
r: Frank = 4;
array[r] of r: first_name = [Jack, Bill, Paul, Frank];

var r: Green;
var r: Clubb;
var r: Sands;
var r: Carter;
array[r] of var r: last_name = [Green, Clubb, Sands, Carter];

var r: cook;
var r: maintenance_man;
var r: clerk;
var r: caddy;
array[r] of var r: job = [cook, maintenance_man, clerk, caddy];

array[r] of var 70..85: score;


solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint
   all_different(last_name) 
   /\
   all_different(job) 
   /\
   all_different(score)

   /\ % 1. Bill, who is not the maintenance man, plays golf often and had 
      % the lowest score of the foursome.
   Bill != maintenance_man
   /\
   score[Bill] < score[Jack] /\
   score[Bill] < score[Paul] /\
   score[Bill] < score[Frank]
 
   /\ % 2. Mr. Clubb, who isn't Paul, hit several balls into the woods and 
      %    scored ten strokes more than the pro-shop clerk.
   Clubb != Paul /\
   score[Clubb] = score[clerk] + 10
   

   /\ % 3. In some order, Frank and the caddy scored four and seven more 
      %    strokes than Mr. Sands.
   Frank != caddy /\
   Frank != Sands /\
   caddy != Sands /\
   (
    (score[Frank] = score[Sands] + 4 /\
     score[caddy] = score[Sands] + 7 )
    \/
    (score[Frank] = score[Sands] + 7 /\
     score[caddy] = score[Sands] + 4 )
   )

   /\ % 4. Mr. Carter thought his score of 78 was one of his better games, even 
      % though Frank's score was lower.
    Frank != Carter /\
    score[Carter] = 78 /\
    score[Frank] < score[Carter]

   /\ % 5. None of the four scored exactly 81 strokes.
   forall(i in r) (
     score[i] != 81
   )
;

output [
  "first_name: ", show(first_name), "\n",
  "last_name : ", show(last_name), "\n",
  "job       : ", show(job), "\n",
  "score     : ", show(score), "\n", 
];
