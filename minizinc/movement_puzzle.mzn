% 
% Movement puzzle in MiniZinc.
% 
% From StackOverflow
% "python recursion: solving movement puzzle"
% http://stackoverflow.com/questions/20011395/python-recursion-solving-movement-puzzle
% """
% im trying to figure out how to write a recursive python program which takes a list
% i.e [1,2,3,4,0] while each number donates [denotes?] the number of steps you can take 
% left or right. and figure out of a way you to get to the zero cell at the end.
% 
% for example [3,6,4,1,3,4,2,5,3,0] , if i start at the leftmost cell(3), i could 
% take: 3 steps right to the 1 cell -> 1 step back to the 4 cell -> 4 steps right 
% to the 2 cell- > 2 steps right to the 3 cell -> 3 steps left to the 4 cell -> and 
% 4 steps right to the 0 cell
% i can start on any cell on the board.. and also need to figure out when it is 
% not possible to solve the board.
% 
% how do i start to think about this using recursion?
% """
%
% This model assume that the same cell is used only once (i.e. that it 
% have just one "direction").
% Also, we allow that we start at any position, including the last.
%
% It also generates random problem instances.
%
% The first example, [1,2,3,4,0], is UNSAT according to this model.
% The second example, [3,6,4,1,3,4,2,5,3,0], has 15 solutions:
% 
%   a: [3, 6, 4, 1, 3, 4, 2, 5, 3, 0]
%   x: [0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
%   c: 1
% ----------
%   a: [3, 6, 4, 1, 3, 4, 2, 5, 3, 0]
%   x: [0, 0, 0, 0, 0, 1, 0, 0, 0, 2]
%   c: 2
% ----------
%   a: [3, 6, 4, 1, 3, 4, 2, 5, 3, 0]
%   x: [0, 0, 0, 0, 0, 2, 0, 0, 1, 3]
%   c: 3
% ----------
%   a: [3, 6, 4, 1, 3, 4, 2, 5, 3, 0]
%   x: [0, 0, 0, 0, 0, 3, 1, 0, 2, 4]
%   c: 4
% ----------
%   a: [3, 6, 4, 1, 3, 4, 2, 5, 3, 0]
%   x: [0, 0, 1, 0, 0, 4, 2, 0, 3, 5]
%   c: 5
% ----------
%   a: [3, 6, 4, 1, 3, 4, 2, 5, 3, 0]
%   x: [0, 0, 2, 1, 0, 5, 3, 0, 4, 6]
%   c: 6
% ----------
%   a: [3, 6, 4, 1, 3, 4, 2, 5, 3, 0]
%   x: [0, 0, 2, 0, 0, 5, 3, 1, 4, 6]
%   c: 6
% ----------
%   a: [3, 6, 4, 1, 3, 4, 2, 5, 3, 0]
%   x: [0, 1, 3, 0, 0, 6, 4, 2, 5, 7]
%   c: 7
% ----------
%   a: [3, 6, 4, 1, 3, 4, 2, 5, 3, 0]
%   x: [0, 0, 3, 0, 1, 6, 4, 2, 5, 7]
%   c: 7
% ----------
%   a: [3, 6, 4, 1, 3, 4, 2, 5, 3, 0]
%   x: [0, 2, 4, 0, 1, 7, 5, 3, 6, 8]
%   c: 8
% ----------
%   a: [3, 6, 4, 1, 3, 4, 2, 5, 3, 0]
%   x: [1, 0, 3, 2, 0, 6, 4, 0, 5, 7]
%   c: 7
% ----------
%   a: [3, 6, 4, 1, 3, 4, 2, 5, 3, 0]
%   x: [0, 0, 4, 1, 2, 7, 5, 3, 6, 8]
%   c: 8
% ----------
%   a: [3, 6, 4, 1, 3, 4, 2, 5, 3, 0]
%   x: [1, 0, 5, 2, 3, 8, 6, 4, 7, 9]
%   c: 9
% ----------
%   a: [3, 6, 4, 1, 3, 4, 2, 5, 3, 0]
%   x: [0, 3, 5, 1, 2, 8, 6, 4, 7, 9]
%   c: 9
% ----------
%   a: [3, 6, 4, 1, 3, 4, 2, 5, 3, 0]
%   x: [1, 4, 6, 2, 3, 9, 7, 5, 8, 10]
%   c: 10
% ----------



% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

% Start position:
%   0 -> no start position required
%  > 0 -> some start position
int: start; 

int: n;
array[1..n] of int: a;


% decision variables


% array[1..n] of var 0..n: a; % for generating problem instance

array[1..n] of var 0..n: x;
var 0..n: c = sum([bool2int(x[i] > 0) | i in 1..n]);

% solve satisfy;
% solve :: int_search(x, max_regret, indomain_random, complete) satisfy; % for generating problems
% solve :: int_search(x, max_regret, indomain_split, complete) satisfy;
% solve :: int_search(x, anti_first_fail, indomain_split, complete) satisfy;
solve minimize c;
% solve :: int_search(x, input_order, indomain_split, complete) minimize c;



%%
%% for generating problems (the a array)
%%
% constraint
%   a[n] = 0 
%   /\
%   forall(i in 1..n-1) ( a[i] > 0)
%   /\
%   forall(i in 1..n-1) (
%      a[i] <= max(n-i,i)
%   )
%   /\ 
%   c >= n div 2
% ;
 

%
% x array
%
constraint
  alldifferent_except_0(x)  
  /\ 
  maximum(x[n], x) 
;

% start position (if any)
constraint 
   if start > 0 then 
      x[start] = 1 
   else 
      true
   endif
;



% The main loop:
% For all entries (x[i] > 0):
%   identify position of last move in x and the 
%   length to the next element.
%
constraint
  x[n] > 0 /\
  forall(i in 2..n) (
    (i <= x[n]) ->
       exists(j,k in 1..n) (
          x[j] = i       /\ 
          x[k] = i-1     /\
          a[k] = abs(j-k)
      )
  )
;

% Using sum() instead of exists()
% constraint
%   x[n] > 0 /\
%   forall(i in 2..n) (
%     (i <= x[n]) ->
%        sum(j,k in 1..n) (
%           bool2int(x[j] = i     /\ 
%                    x[k] = i-1   /\
%                    a[k] = abs(j-k)
%                   )
%            ) > 0 
%   )
% ;


%% connect at the end as well
%% There must be a (proper) last step which is
%% connected to x[n].
constraint
  exists(j in 1..n-1) (
     x[j] = x[n]-1 /\
     a[j] = n - j
  )
;

%% as tmp variable
% constraint
%   let {
%     var 1..n: j
%   } in
%      x[j] = x[n]-1 /\
%      a[j] = n-j
% ;

%% using sum()
% constraint
%   sum(j in 1..n) ( bool2int(
%      x[j] = x[n]-1 /\
%      a[j] = n-j
%       )) > 0
% ;




output 
[
   "%  a: " ++ show(a) ++ "\n" ++
   "%  x: " ++ show(x) ++ "\n" ++
   "%  c: " ++ show(c) ++ "\n"
]
;



%
% data
%

%  3 solutions for start = 1
% Total 15 solutions for start = 0
% n = 10;
% a = [3,6,4,1,3,4,2,5,3,0];
% start = 0;

% This problem is unsat for any start
% n = 5;
% a = [1,2,3,4,0];
% start = 0;

% No solution of start = 1, 
% 3  solution if no start requirement
% n = 5;
% a = [3,1,2,3,0];
% % start = 1;
% start = 0;


%%
%% generated problem
%%

% 10 solutions
% 1 solution for start = 1
% n = 5;
% a = [4, 1, 2, 2, 0];
% start = 1;

% 4 solutions for start = 1
% 27 solutions for any start
% n = 10;
% a = [5, 7, 1, 6, 2, 2, 3, 6, 4, 0];
% start = 0;

% 1 solution for start=1
% In all 22 solutions
% n = 20;
% a = [14, 11, 8, 16, 9, 11, 3, 3, 10, 7, 2, 4, 3, 13, 3, 10, 7, 11, 7, 0];
% start = 0;

% a lot of solutions
n = 40;
a = [8, 28, 22, 27, 28, 2, 29, 9, 30, 7, 5, 10, 8, 21, 13, 3, 10, 13, 1, 6, 11, 16, 17, 17, 15, 8, 2, 5, 5, 8, 3, 31, 14, 3, 23, 32, 27, 27, 24, 0];
start = 1;


% n = 50;
