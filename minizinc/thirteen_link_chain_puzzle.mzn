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
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn";

int: n = 13;
int: m = if n >= 10 then 1 + (n div 2) else n endif;

% the weights
% Note: that this representation excludes 
% two chains (i.e. numbers) of the same length (value).
var set of 1..m: weights;

array[1..n] of var set of 1..m: s;
var 1..m: t = card(weights);


solve minimize t;
% solve :: set_search(s, first_fail, indomain_max, complete) minimize t;
% solve satisfy;

constraint
  trace("n: " ++ show(n) ++ " m: " ++ show(m) ++ "\n", 1=1)
;

% set based weights
constraint
        % Ensure that the length of the chain is n.
        % 
        % note: this constraint (together with the set representation)
        %       makes it impossible to split chains of following length:
        %            2,4,5,8,9
        %       However:
        %       2 can be split in two 1's: 1 and 1+1
        %       4 can be split in four 1's: 1,1+1,1+1+1,1+1+1+1
        %       5              in five 1's
        %       8              in eight 1's
        %       9              in nine 1's
        sum(i in 1..m) ( i*bool2int(i in weights)) = n /\

        forall(j in 1..n) (
           sum(i in 1..m) (
                 i*bool2int(i in s[j] /\ i in weights)
             ) = j 
        )
        /\ % skip all the items not needed in s[k]
        forall(j in 1..n) (
          not( j in weights) <->
            forall(k in 1..n) ( not(j in s[k])) 
        )

       % /\ 1 in weights /\ 1 in s[1]
;

% I'm not sure if this always works, but
% when it does it speeds things up considerably.

% It seems to work for n > 25
% For n <= 25:
% n  works   
% 1 1
% 2 0   Never works with m
% 3 1
% 4 0   Never works with m
% 5 0   Never works with m
% 6 0   1
% 7 1   1
% 8 0   Never works with m
% 9 0   Never works with m
% 10 0
% 11 0
% 12 0
% 13 0
% 14 1 
% 15 1
% 16 0
% 17 0 
% 18 0
% 19 0 
% 20 0
% 21 1
% 22 0
% 23 0
% 24 1
% 25 0
constraint  
  % set based
  if n > 25 then 
    forall(i, j in 1..m where i < j) (
      (i in weights /\ j in weights) -> not(i+j in weights)
    )
  else 
    true
  endif
;

output 
[ 
 "weights: " ++ show(weights) ++ "\n" ++
 "s: " ++ show(s) ++ "\n" ++
 "t: " ++ show(t) ++ "\n"
]
% ++
% [
%   show(i) ++ ": " ++ show(s[i]) ++ "\n"
%   | i in 1..n
% ]
;