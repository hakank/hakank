% 
% Car Talk Odometer problem in MiniZinc.
% 
% From http://www.cartalk.com/content/tommy%E2%80%99s-drive-work?question
% """
% ...
% RAY: Well, not exactly new, it's 19 years old. But it's new enough to have lots of things 
% that his other car didn't have. It has heat, lights, and windshield wipers. It also has one of those 
% newfangled six-digit odometers. It can register as many as 999,999 miles.
% 
% So one fine morning last week, Tommy gets into his new car to drive to work. He fires up his engine 
% and before pulling out of the driveway, he notices something interesting. His odometer reading is 
% a palindrome. Do you know what a palindrome is?
%
% TOM: No.
%
% RAY: It's a number that reads the same forwards as it does backwards. For example, if his 
% odometer read 175,571 miles, that's a palindrome number. 1 - 7 - 5, 5 - 7 -1, reads the same 
% forward and backwards. 'Well, that's cool,' he says to himself, and off he goes.
%
% Naturally, he's in no hurry to get to work, so he stops at his favorite cafe and gets his usual 
% quadruple espresso macchiato, and about an hour later, he shows up at Car Talk Plaza. Just as he 
% pulls into his 'Reserved for the Big Cheese' parking spot, he notices that his odometer reading 
% again is a palindrome. And no, the odometer is not broken, but it is indeed a palindrome 
% number once again.
% 
% So he's gotten in the morning, seen a number, driven some number of miles - not many, cause he only 
% drove for an hour,--not even, most of the time he was drinking coffee -- and his new odometer 
% reading is again a palindromic number.
%
% Here's the question: How far did he drive that morning?
%
% """

% Answer: http://www.cartalk.com/content/tommy%E2%80%99s-drive-work?answer

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 6;

% decision variable
var 100000..999999: start;
var 100000..999999: end;
array[1..n] of var 0..9: start_a;
array[1..n] of var 0..9: end_a;

var int: z = end - start;

function var int: to_num(array[int] of var int: a) =
          let { int: len = card(index_set(a));
                var int: n = sum(i in index_set(a)) (
                   pow(10, len-i) * a[i] 
                 );
         } in n
;


%
% Ensure that t is a palindromic number.
% (We assume that the are no leading 0's.)
%
predicate is_palindromic(array[int] of var int: t) = 
   let {
     int: m = length(t)
   } in
   forall(i in index_set(t)) (
      t[i] = t[m-i+1]
   )
;


% solve satisfy;
solve :: int_search(start_a ++ end_a, first_fail, indomain_min, complete) minimize z;

constraint
  start = to_num(start_a) /\
  end = to_num(end_a) /\

  is_palindromic(start_a) /\
  is_palindromic(end_a) /\

  z > 0

  % /\ z = 11
;

output [
  "start: \(start) .. end: \(end) z: \(z)\n"
];

