% 
% KenKen problem in MiniZinc.
% 
%   http://en.wikipedia.org/wiki/KenKen
%   """
%   KenKen or KEN-KEN is a style of arithmetic and logical puzzle sharing
%   several characteristics with sudoku. The name comes from Japanese and
%   is translated as "square wisdom" or "cleverness squared".
%   ...
%   The objective is to fill the grid in with the digits 1 through 6 such that:

%     * Each row contains exactly one of each digit
%     * Each column contains exactly one of each digit
%     * Each bold-outlined group of cells is a cage containing digits which
%       achieve the specified result using the specified mathematical operation:
%         addition (+),
%         subtraction (-),
%         multiplication (x),
%         and division (÷).
%         (Unlike in Killer sudoku, digits may repeat within a group.)

%   ...
%   More complex KenKen problems are formed using the principles described
%   above but omitting the symbols +, -, x and ÷, thus leaving them as
%   yet another unknown to be determined.
%   """
%
%   The solution is:
%
%     5 6 3 4 1 2
%     6 1 4 5 2 3
%     4 5 2 3 6 1
%     3 4 1 2 5 6
%     2 3 6 1 4 5
%     1 2 5 6 3 4

%
% Also, see the Comet models
% http://www.hakank.org/comet/kenken2.co
% http://www.hakank.org/comet/kenken.co (less general version)
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 6;
array[1..n, 1..n] of var 1..n: x;

%
% state the problem (without the operation)
%
% For a better view of the problem, see
%  http:%en.wikipedia.org/wiki/File:KenKenProblem.svg
%
int: num_p = 15; % number of segments
int: num_hints = 4;  % number of hints per segments (that's max number of hints)
array[1..num_p, 1..2*num_hints+1] of int: P =
  array2d(1..num_p, 1..2*num_hints+1, [
   1,1, 2,1, 0,0, 0,0,   11,
   1,2, 1,3, 0,0, 0,0,   2,
   1,4, 2,4, 0,0, 0,0,  20,
   1,5, 1,6, 2,6, 3,6,   6,
   2,2, 2,3, 0,0, 0,0,   3,
   2,5, 3,5, 0,0, 0,0,   3,
   3,1, 3,2, 4,1, 4,2, 240,
   3,3, 3,4, 0,0, 0,0,   6,
   4,3, 5,3, 0,0, 0,0,   6,
   4,4, 5,4, 5,5, 0,0,   7,
   4,5, 4,6, 0,0, 0,0,  30,
   5,1, 5,2, 0,0, 0,0,   6,
   5,6, 6,6, 0,0, 0,0,   9,
   6,1, 6,2, 6,3, 0,0,   8,
   6,4, 6,5, 0,0, 0,0,   2
   ]);


%
% This predicate is needed since the built-in product don't work.
%
predicate prodx(array[int] of var int: R, var int: p, var bool: b) = 
  let {
     int: lbx = min(index_set(R)),
     int: ubx = max(index_set(R)),
     array[lbx..ubx] of var 0..1000: a
  }
  in

  a[lbx] = R[lbx]
  /\ 
  forall(i in lbx+1..ubx) ( 
        a[i] = a[i-1]*R[i]
  )
  /\
  (p = a[ubx]) = b % reification trickery

;


% solve satisfy;
solve :: int_search([x[i,j] | i,j in 1..n], smallest, indomain_max, complete) satisfy;

constraint

  forall(i in 1..n) (
     all_different([x[i,j] | j in 1..n]) /\
     all_different([x[j,i] | j in 1..n]) 
  )
  /\
  forall(p in 1..num_p) (
    let {
       int: res = P[p, 2*num_hints+1]
    } in

    if sum(i in 1..2*num_hints) (bool2int(P[p,i] > 0)) == 4 then
      % is it just two numbers?
      let {
         var 1..n: a,
         var 1..n: b
      } in
      a = x[P[p,1], P[p,2]] /\
      b = x[P[p,3], P[p,4]] /\
       (
           (a + b = res) \/
           (a * b = res) \/
           (a * res = b) \/
           (b * res = a) \/
           (a - b = res) \/
           (b - a = res)
       )
    else 
           % Note: some trickery here is needed since otherwise an 
           % error of reified/negative context is thrown on prodx
           let {
             var bool: bb
           } in
           (sum(i in 1..num_hints where P[p,2*(i-1)+1] > 0) (x[  P[p, 2*(i-1)+1], P[p,2*(i-1)+2]  ]) = res <-> bb = false)
           /\
           % (product([x[  P[p, 2*(i-1)+1], P[p,2*(i-1)+2]  ] | i in 1..num_hints where P[p,2*(i-1)+1] > 0]) = res) % this don't work! ("internal error in predicate")
          (prodx([x[  P[p, 2*(i-1)+1], P[p,2*(i-1)+2]  ] | i in 1..num_hints where P[p,2*(i-1)+1] > 0], res, bb))

     endif
  )
;


output [
  if j = 1 then "\n" else " "  endif ++
    show(x[i,j])
  | i,j in 1..n
];

