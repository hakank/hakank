% 
% Fizz Buzz in MiniZinc.
% 
% http://codingdojo.org/cgi-bin/wiki.pl?KataFizzBuzz
% """
% Write a program that prints the numbers from 1 to 100. But for multiples of 
% three print "Fizz" instead of the number and for the multiples of five 
% print "Buzz". For numbers which are multiples of both three and five print 
% "FizzBuzz?". 
% ...
% Stage 2 - new requirements
% * A number is fizz if it is divisible by 3 or if it has a 3 in it
% * A number is buzz if it is divisible by 5 or if it has a 5 in it
% """
%
% Note: The stage 2 is incomplete. It probably means:
% * if it divisible by 3 and has a 5 in it -> fizzbuzz
% * if it divisible by 5 and has a 3 in it -> fizzbuzz

% Note2:
% The solution below is more complicated than an if ... elseif solution
% since we must explicity handle all the cases at the same time, i.e. there is
% no "order" between the AND clauses as in the if ... elseif construct.
% And, we can't use the if approach since MiniZinc don't allow var variables
% in those constructs (as the temp array a).

% Note3:
% This version of FizzBuzz is slightly different to other versions since
% it adds the requiremen "or if it has a <n> in it".

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 


int: n = 100;

% 0: nothing
% 1: fizz
% 2: buzz
% 3: fizzbuzz
array[1..n] of var 0..3: x; % 


%
% array <-> number
%
predicate toNum(array[int] of var int: number, var int: num_sum) =
          let { int: len = length(number) }
          in
          num_sum = sum(i in 1..len) (
            ceil(pow(10.0, int2float(len-i))) * number[i]
          )
          /\ forall(i in 1..len) (number[i] >= 0)
;


%
% does a contains e?
%
predicate contains(var int: e, array[int] of var int: a) =
   exists(i in 1..length(a)) (
      a[i] = e
   )
;

array[0..3] of string: fizz_buzz = array1d(0..3, ["", "fizz", "buzz", "fizzbuzz"]);

%solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;


constraint
   forall(i in 1..n) (
     let {
        array[1..3] of var 0..9: a
     }
     in
     toNum(a, i)
     /\
     (
      ( 
        (i mod 3 = 0 /\ i mod 5 = 0) 
         \/ 
        (contains(3, a) /\ contains(5, a))
         \/
        (i mod 3 = 0 /\ contains(5, a)) 
         \/
        (i mod 5 = 0 /\ contains(3, a)) 

       )
        <-> x[i] = 3  % fizz buzz
     )
     /\
     (
      (
       (i mod 3 = 0 \/ contains(3, a) )
       /\
       (i mod 5 > 0 /\ not(contains(5, a) ))
      )
      <-> x[i] = 1  % fizz
     )
     /\
     (
      (
       (i mod 5 = 0 \/ contains(5, a)) 
       /\
       (i mod 3 > 0 /\ not(contains(3, a) ))
      )
      <-> x[i] = 2 % buzz
     )
     /\
     (
       (
        i mod 3 > 0 /\ i mod 5 > 0 /\ 
        not(contains(3,a)) /\ not(contains(5,a))
       )
        <-> x[i] = 0
      )


   )
;

output 
% [
%   show(i) ++ ": " ++ show(x[i]) ++ "\n"
%   | i in 1..n
% ] ++ 
[
  let {
    int: t = fix(x[i])
  } in
    show(i) ++ ": " ++
    if t = 0 then show(i) else show(fizz_buzz[t]) endif ++
    "\n"
  | i in 1..n
] ++
["\n"];