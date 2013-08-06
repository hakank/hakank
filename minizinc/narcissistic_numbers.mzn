% 
% Narcissistic Numbers in MiniZinc.
%
% From http://mathworld.wolfram.com/NarcissisticNumber.html
% """
% An n-digit  number that is the sum of the nth powers of its digits  
% is called an n-narcissistic number. It is also sometimes known as an 
% Armstrong number, perfect digital invariant (Madachy 1979), or plus 
% perfect number. 
% 
% Hardy (1993) wrote, "There are just four numbers, after unity, which are 
% the sums of the cubes of their digits: 153=1^3+5^3+3^3, 370=3^3+7^3+0^3, 
% 371=3^3+7^3+1^3, and 407=4^3+0^3+7^3. These are odd facts, very suitable 
% for puzzle columns and likely to amuse amateurs, but there is nothing in 
% them which appeals to the mathematician." Narcissistic numbers therefore 
% generalize these "unappealing" numbers to other powers 
% (Madachy 1979, p. 164).
% """

%
% MiniZinc model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
% 


int: n = 4;
int: base = 10;
array[1..n] of var 0..base-1: x;
var int: num; % base-ary representation of x

%
% convert a number to array, and vice versa
%
predicate toNum(array[int] of var int: tal, var int: summa,  float: base) =
    let { int: len = length(tal) }
    in
    summa = sum(i in 1..len) (
      ceil(pow(base, int2float(len-i))) * tal[i]
    )
    /\ 
    forall(i in 1..len) (tal[i] >= 0)
;

%
% pow() don't handle (var int, int)
%
predicate my_pow(var int: nn, int: len, var int: n_pow) =
   let {
      array[1..len+1] of var int: xx
   }
   in
   xx[1] = 1
   /\
   forall(i in 2..len+1) (
     xx[i] = xx[i-1] * nn
   )
   /\
   n_pow = xx[len+1]
;

% solve satisfy;
solve :: int_search(x, first_fail, indomain_split, complete) satisfy;

constraint
    let { 
      array[1..n] of var int: pows
    }
    in
    num > 0
    /\
    x[1] > 0
    /\ 
    toNum(x, num, int2float(base))

    /\ % create the n'th power array
    forall(i in 1..n) ( 
      my_pow(x[i], n, pows[i])
    )
    /\ % sum of n'th powers
    num = sum(pows)
;


output [
  "num: ", show(num), "\n",
  "x: ", show(x), "\n"
];
