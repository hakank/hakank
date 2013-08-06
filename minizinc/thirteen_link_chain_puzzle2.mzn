%
% The 13-Link Chain Puzzle in Minzinc
% 
% From
% http://wordplay.blogs.nytimes.com/2012/10/29/chain/
% """
% The 13-Link Chain Puzzle
% You have a balance scale and a single chain with thirteen 
% links. Each link of the chain weighs one ounce. How many links 
% of the chain do you need to break in order to be able to weigh 
% items from 1 to 13 ounces in 1-ounce increments?
% """
%
% Discussion: http://ayecapitalist.com/2012/10/29/the-13-link-chain-puzzle-and-other-fun-posers/#comments
%
% 
% This is an array based general approach.
% And I realize that I have not understood the problem correctly:
% it's a balance scale so we can use +/- to get each weight...
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn";

int: n = 13;

int: m1 = if n >= 10 then 1 + (ceil(sqrt(int2float(n)))) else n endif;
int: m2 = if n >= 10 then 1 + (n div 2) else n endif;
int: m3 = if n >= 10 then 1 else 2 endif; % domain of x

% the weights
array[1..m1] of var 0..m2: weights;
array[1..n, 1..m1] of var 0..m3: x; % ..m3: x;

var 0..m1: t = sum(j in 1..m1) ( bool2int(weights[j]>0) );
var 0..m2: t2 = max([x[i,j] | i in 1..n, j in 1..m1]);

solve :: int_search(
                weights, %  ++ 
                % [x[i,j] | i in 1..n, j in 1..m1], 
                max_regret, % first_fail, 
                indomain_min, 
                complete) 
            % minimize t;
            % minimize t2;
            % minimize t + t2;
            minimize t * t2;
            % minimize weights[m1];
            % satisfy;
% solve satisfy;

constraint
        sum(j in 1..m1) ( max([x[i,j] | i in 1..n])*weights[j] ) = n
        /\
        forall(i in 1..n) (
           i = sum(j in 1..m1) ( x[i,j]*weights[j] )
        )

        /\ % different symmetry breaking and speed ups
        increasing(weights) :: domain
        /\ 
        alldifferent_except_0(weights) :: domain
        /\ 
        forall(j in 1..m1) (
          weights[j] = 0 <-> forall(i in 1..n) (x[i,j] = 0)
        )
;

%
% Kind of symmetry breaking (for n > 25):
% Ensure that two selected weights cannot be added to 
% another selected weight.
%
% constraint
%    if n > 25 then
%       forall(i, j in 1..m1 where i < j) (
%          (weights[i] > 0 /\ weights[j] > 0) -> 
%          not(exists(k in 1..m1) ( weights[k] = weights[i] + weights[j]))
%       )
%    else 
%      true
%    endif
% ;

% Symmetry breaking
constraint 
  if n > 100 then
    forall(i in 2..m1) (
      weights[i] >= weights[i-1]*2-1
    )
  else 
    true
  endif
;


output 
[ 
  "weights: " ++ show(weights) ++ "\n"
]
++
[ 
  if j = 1 then "\n" ++ show_int(4, i) ++ ": "  else " " endif ++
    if fix(weights[j]) > 0 then
       show_int(2, x[i,j])
    else 
       ""
    endif
  | i in 1..n, j in 1..m1
]
++
[ 
 "\nweights: " ++ show(weights) ++ "\n" ++
 "t: " ++ show(t) ++ "\n" ++
 "t2: " ++ show(t2) ++ "\n" 
]
;