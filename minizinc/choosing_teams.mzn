% 
% Choosing teams in MiniZinc.
% 
% http://blogs.msdn.com/b/oldnewthing/archive/2014/08/04/10547079.aspx
% """
%  Suppose you have a bunch of people, and you want to break them up into 
% m teams of size n. (Therefore you have a total of nm people.) Today's Little 
% Program will enumerate the ways this can be done.
% 
% Formally, let's say that you have a collection of size nm, and you want to 
% enumerate the ways of partitioning the collection into m subsets, each 
% subset of size n. The order of elements within each subset does not 
% matter, and the order of the subsets doesn't matter. That's saying that a 
% team of Alice and Bob is the same as a team of Bob and Alice, and Alice-Bob 
% versus Charlie-David is the same as Charlie-David versus Alice-Bob. 
% 
% The number of ways of doing this is (nm)!/n!mm!. You can see this by first 
% taking all permutations of the players, then dividing out by the things that 
% cause us to overcount: The number of ways of ordering players within each team 
% is n!, and there are m teams, and there are m! ways of ordering the teams 
% themselves. (Note that this is a cute way of expressing the result, but you 
% shouldn't use it for computation. A slightly better way for computation would 
% be (Π1 ≤ k ≤ nC(mk, m))/m!. 
% """

% Note: The function factorial requires MiniZinc 2.0. 
%       To be able to use it in MiniZinc 1.6: 
%        - comment the function definition
%        - comment the trace constraint
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: m = 4; % number of teams
int: n = 4; % size of team

int: p = m*n; % number of people: m*n

%
% (n*m)! /(((n!)^m)*m!)
% 
% m   n   #sol
% -----------
% 1  1  1
% 1  2  1
% 1  3  1
% 1  4  1
% 2  1  1
% 2  2  3
% 2  3  10
% 2  4  35
% 3  1  1
% 3  2  15
% 3  3  280
% 3  4  5775
% 4  1  1
% 4  2  105
% 4  3  15400
% 4  4  2627625


% decision variables
array[1..m, 1..n] of var 1..p: x;

% solve satisfy;
solve :: int_search([x[i,j] | i in 1..m, j in 1..n], first_fail, indomain_min, complete) satisfy;

function int: factorial(int: n) =  product([i | i in 1..n]);

constraint
   trace("m: " ++ show(m) ++ " n: " ++ show(n) ++ ": " ++ 
         show(factorial(n*m) div (( pow(factorial(n),m)*factorial(m)))) ++ "\n", true)
;

constraint
  all_different([x[i,j] | i in 1..m, j in 1..n])

  % symmetry breaking
  /\ lex2(x) 
  /\ 
  forall(i in 1..m) (
    increasing([x[i,j] | j in 1..n])
  )
;

output [
  if j = 1 then "\n" else " " endif ++
    show_int(3, x[i,j])
  | i in 1..m, j in 1..n
];

