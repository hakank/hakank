% 
% All paths (with a graph representation) in MiniZinc.
% 
% Problem from http://www.anselm.edu/homepage/mmalita/logic/message.txt
% """
% Email is out of order at St. Mary's College and the teacher wants 
% to tell Robert something urgent.
% The teacher meets Craig and asks him to tell Robert she wants to speak 
% with him. 
% Craig says that if he meets Robert its OK, but else he will send the message 
% to everyone he meets and the message will go further.
% Each student tells each student he meets that the teacher waits for Robert
% in her office.
% The students meet each other (we don't know in what order):
% 1) Craig meets John and Jason
% 2) Jason meets Kiki and Adam and David
% 3) Adam meets Scott and Jeremy
% 4) Jeremy meets John and Scott
% 5) Kiki meets Chris
% 6) Chris meets David and Adam
% 7) David meets Robert
%
% Do you think the teacher will wait for ever in her office?
% Display all the possible paths from Craig to Robert.
%
% """
% 

% This model is derived from the model message_sending.mzn.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n = 10;
set of int: N = 1..n;
int: num_edges = 13;

int: Adam = 1;
int: Chris = 2;
int: Craig = 3;
int: David = 4;
int: Jason = 5;
int: Jeremy = 6;
int: John = 7;
int: Kiki = 8;
int: Robert = 9;
int: Scott = 10;

array[N] of var 0..n: x; % 0 is no edge
array[1..num_edges, 1..2] of N: graph;
var int: path_len = sum(i in N) (bool2int(x[i] > 0));

%
% all values are different (range 1..n) or 0
%
predicate all_different_except_0(array[int] of var int: x) =
  let {
      int: n = length(x)
   }
   in
   forall(i,j in 1..n where i != j) (
        (x[i] > 0 /\ x[j] > 0) -> x[i] != x[j]
   )
;


%
% y is the last element which is > 0. All elements after y is 0
%
predicate last_not_0(array[int] of var int: x, var int: y) =
   exists(i in N) (
      x[i] = y
      /\ % all elements in x after y are 0
      forall(j in i+1..n) (x[j] = 0)
      /\ % all elements in x before y are > 0
      forall(j in 1..i) (x[j] > 0)
   )
;

%
% all_paths
% 
% (Well, all paths are generated only if the solver generates all
%  possible solutions.)
%
predicate all_paths(array[int] of var int: x, 
                    array[int, 1..2] of var int: graph) =
   forall(i in 2..length(x)) (
     x[i] = 0
     \/
     (
       x[i] > 0
       /\
       x[i-1] > 0
       /\
       exists(j in index_set_1of2(graph)) (
         ( graph[j,1] = x[i-1] /\ graph[j,2] = x[i]   )
         \/
         ( graph[j,1] = x[i]   /\ graph[j,2] = x[i-1] )
       )
     )
   )
;

% solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

% as a shortest path problem:  
solve :: int_search(x, first_fail, indomain_min, complete) minimize path_len;


constraint
   all_different_except_0(x) 
   /\
   x[1] = Craig
   /\
   last_not_0(x, Robert)
   /\
   all_paths(x, graph)
;

%
% The graph
%
graph = array2d(1..num_edges, 1..2, 
  [
   Craig, John,
   Craig, Jason,
   Jason, Kiki,
   Jason, Adam, 
   Jason, David,
   Adam, Scott, 
   Adam, Jeremy,
   Jeremy, John,
   Jeremy, Scott,
   Kiki, Chris,
   Chris, David,
   Chris, Adam,
   David, Robert
  ]);


output
[
  "x: " ++ show(x) ++ "\n" ++
  "path_len: " ++ show(path_len) 
];