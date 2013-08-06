% 
% Alien tiles puzzle in MiniZinc.
%

% Problem and original OPL model in
% Martin Chlond: "An Alien IP"
% http://archive.ite.journal.informs.org/Vol7No1/Chlond/
% 
% OPL Model in
% http://archive.ite.journal.informs.org/Vol7No1/Chlond/alien.php
% The model have the following note: 
% Model name   : alien.mod
% Description  : solves Alien tiles puzzles
% Source       : www.pickover.com - Clifford Pickover 
% Date written : 14/6/06
% Written by   : Martin Chlond, Lancashire Business School
% Email        : mchlond@uclan.ac.uk

% 
% This Minizinc model was written by Hakan Kjellerstrand, 
% hakank@bonetmail.com,
% See also my MiniZinc page: http://www.hakank.org/minizinc
% 

% According to 
% http://archive.ite.journal.informs.org/Vol7No1/Chlond/
% This is the solution
%   0 1 0 0 0 1 0
%   1 2 0 1 0 2 1
%   0 0 3 0 0 3 0
%   0 1 0 1 0 1 0
%   0 0 3 0 3 0 0
%   1 2 0 1 0 2 1
%   0 1 0 0 0 1 0
 
int: n = 7; 
set of int: N = 1..n;

%  compute ranges for symmetry constraints 
int: fhr = n div 2;
int: shr = if n mod 2 = 0 then fhr+1 else fhr+2 endif;

% start position
array[N,N] of 0..1: s = array2d(N, N, [0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0]);

% target position                               
array[N,N] of 0..1: t = array2d(N,N, [0,0,0,1,0,0,0,
                                      0,0,1,1,1,0,0,
                                      0,1,1,1,1,1,0,
                                      1,1,1,1,1,1,1,
                                      0,1,1,1,1,1,0,
                                      0,0,1,1,1,0,0,
                                      0,0,0,1,0,0,0]);

array[N, N] of var 0..3: x;
array[N, N] of var 0..10: d;

predicate cp2d(array[int,int] of var int: x, array[int,int] of var int: y) =
  assert(index_set_1of2(x) = index_set_1of2(y) /\
         index_set_2of2(x) = index_set_2of2(y),
           "cp2d: x and y have different sizes",
     forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
         y[i,j] = x[i,j]
    ) 
  )
; 

  

solve :: int_search( 
        [x[i,j] | i,j in N], % [d[i,j] | i,j in N], % 
        most_constrained, 
        indomain_min, 
        complete) 
    % minimize sum(i in N, j in N) (x[i,j]);
    satisfy;


% solve satisfy;

constraint
  forall(i,j in N) (
    sum(l in N) (x[i,l]) + sum(k in N where k != i) (x[k,j]) = 4*d[i,j]+t[i,j]%-s[i,j]
  )
   
  /\ %  eliminate symmetries 
  sum( i in 1..fhr, j in N) (x[i,j]) <= sum( i in shr..n, j in N) (x[i,j])
   /\
  sum( i in N, j in 1..fhr) (x[i,j]) <= sum( i in N, j in shr..n) (x[i,j])
;

% constraint 
%    % solution
%   cp2d(x, array2d(N,N, [
%                    _,_,_,_,_,_,_,
%                    _,_,_,_,_,_,_,
%                    _,_,_,_,_,_,_,
%                    _,_,_,_,_,_,_,
%                    _,_,_,_,_,_,_,
%                    _,_,_,_,_,_,_,
%                    _,_,_,_,_,_,_

%                    % 0,1,0,0,0,1,0,
%                    % 1,2,0,1,0,2,1,
%                    % 0,0,3,0,0,3,0,
%                    % 0,1,0,1,0,1,0,
%                    % 0,0,3,0,3,0,0,
%                    % 1,2,0,1,0,2,1,
%                    % 0,1,0,0,0,1,0
%        ]))
% ;


output [
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i,j in N
];