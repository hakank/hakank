% 
% Seating problem of OR'ers in MiniZinc.
% 
% This problem was inspired by the matching of OR'ers by
% Figure It Out's "Cupids with Computers" (Capgemini blog)
% http://blogs.uk.capgemini.com/orblog/2011/02/25/cupids-with-computers/
% 
% where a bunch of OR'ers (including me) was matched according to some
% - not exactly stated - compatibility scores.
% """
% Whatever the method, we thought we would have a go ourselves at 
% matchmaking International OR bloggers. Over the past few months INFORMS 
% (the US society for Operational Research) has been running a blogging 
% challenge. The’ve designated a topic and let the international 
% OR Community do their best.  December was 'OR and Holidays'; January was 
% 'OR and Politics' and February is 'OR and Love'.  Of the bloggers who 
% wrote articles for the December and January INFORMS Blog Challenges, 
% who is most compatible? How could we have ensured that nobody was 
% alone for Valentine’s day?
% 
% At this point we would like to apologise if we have offended any of 
% our bloggers by including them in our matchmaking exercise – we hope 
% people understand that this is only an exercise, though who couldn’t 
% use a new friend?
% 
% We took the 19 participants, and built our dataset: Blog platform, 
% twitter presence, occupation, blog challenge topics and participation, 
% location, blog popularity, etc. Note that gender is NOT a parameter 
% as we are being Platonic Cupid here, seeking to ensure that our bloggers 
% are not alone on Valentine’s Day. After consulting our people at our 
% own Relationship Science Research Centre of Excellence, we established a 
% weighted algorithm for measuring compatibility. We were now prepared 
% to start delivering recommendations.
% """

% The 19 x 19 score matrix inspired me of a kind of related problem: 
% When all these persons meet, what is the best way to place them around 
% a (toplogical circular) table if the object is to get the highest 
% possible compatibility score for the adjacent seated persons.
%
%
% The two optimal solutions (total=996) is:
% 
% Using my_circuit/2:
% These paths always ends at 1, due to the method in my_circuit/2:
%   z: 9 10 18 8 16 13 15 12 3 17 19 2 11 6 14 5 7 4 1
%   z: 4 7 5 14 6 11 2 19 17 3 12 15 13 16 8 18 10 9 1
%
%
% Using circuit/1 and circuit_path/2:
% These paths always starts at 1.
% z: 1 9 10 18 8 16 13 15 12 3 17 19 2 11 6 14 5 7 4
% z: 1 4 7 5 14 6 11 2 19 17 3 12 15 13 16 8 18 10 9

% Note: It requires a solver with support for circuit/1.% 
% As of writing (2011-03) only Gecode/fz and JaCoP/fz has
% support for circuit/1.

%
% Compare with the matching using the same score matrix in 
%   http://www.hakank.org/minizinc/or_matching.mzn
%    1  9 
%   10 18 
%    5 14 
%    8 16 
%    2 19 
%    3 12 
%    6 11 
%   13 15 
%    4  7 
% 
% Note that 17 is not included in the matching.


% For Gecode and JaCoP it is recommended to use
%   circuit(x) /\
%   circuit_path(x, p)
% Don't forget to include the "<solver>".mzn file.
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

% if using circuit/1, use one of these
% include "gecode.mzn";
% include "jacop.mzn";

int: n = 19;
array[1..n, 1..n] of int: scores;
int: max_score = max([scores[i,j] | i, j in 1..n]);

% decision variables: the circular table
% Note that the seating is the z array
array[1..n] of var 1..n: x; % the circuit
array[1..n] of var 1..n: z; % the seating we want

% the scores for each left neighbour
array[1..n] of var 0..max_score: s; 

% sum of s, to maximize
var 0..n*max_score: total = sum(s);

%
% This is a variant of my decomposition of circuit/1
% ( http://www.hakank.org/minizinc/circuit_me.mzn )
% where we use the helper array z to extract the path
% from the circuit
% - x is a circuit.
% - z is the corresponding placements 
%   (the path in the circuit)
%
predicate circuit_me(array[int] of var int: x, array[int] of var int: z) =
  let { 
        int: lbx = min(index_set(x)),
        int: ubx = max(index_set(x))
  }
  in
   all_different(x) :: domain /\
   all_different(z) :: domain  /\

   % put the orbit of x[1] in in z[1..n]
   z[lbx] = x[lbx] /\ 
   forall(i in lbx+1..ubx) (
      z[i] = x[z[i-1]]
   )
   /\ % may not be 1 for i < n
   forall(i in lbx..ubx-1) (
      z[i] != lbx
   )
   /\  % when i = n it must be 1
   z[n] = lbx
;

%
% circuit_path/2: Extract the path (p) 
% from circuit(x).
%
predicate circuit_path(array[int] of var int: x, 
                       array[int] of var int: p) =

   % we always starts the path at 1
   p[1] = 1
   /\
  forall(i in 2..n) (
    p[i] = x[p[i-1]] 
  )

; 


% solve satisfy;
solve :: int_search(
        x ++ z,
        first_fail, 
        indomain_min, 
        complete) 
    maximize total;
% solve maximize total;
% solve :: int_search(s, largest, indomain_max, complete) satisfy;

% constraint alldifferent(x) :: domain;

constraint
    % Using a solver's implementation of circuit/1
    % circuit(x)  /\

    % Using my decomposition of circuit
    % with z as the seating (path)
    % Note: circuit_me has the (implicit) symmetry breaking
    % constraint that the last element in z (i.e. the path) 
    % ends in 1.
    circuit_me(x, z) 
    /\
    forall(i in 1..n) (
        s[i] = scores[i, x[i]]
    )
;


% For solve satisfy
% constraint total = 996;


% Note: the scores are really from 1.0 to 9.2 but since many solves 
% don't support floats I multiplied by 10 so the range is from 10 to 92,
% and 0 is for the diagonals.
scores = array2d(1..n, 1..n, 
  [
%  1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19
  00, 30, 10, 33, 49, 50, 38, 32, 92, 61, 31, 22, 31, 55, 26, 19, 21, 61, 14, %  1
  30, 00, 21, 23, 10, 33, 28, 15, 22, 22, 36, 33, 36, 38, 36, 24, 26, 22, 55, %  2
  10, 21, 00, 10, 22, 26, 10, 22, 10, 41, 26, 52, 21, 24, 21, 33, 33, 10, 33, %  3
  33, 23, 10, 00, 24, 25, 37, 24, 36, 36, 22, 22, 22, 22, 22, 10, 13, 36, 10, %  4
  49, 10, 22, 24, 00, 41, 55, 42, 57, 57, 16, 15, 21, 69, 16, 33, 31, 57, 23, %  5
  50, 33, 26, 25, 41, 00, 25, 19, 53, 53, 44, 33, 39, 61, 34, 27, 30, 53, 22, %  6
  38, 28, 10, 37, 55, 25, 00, 29, 36, 36, 22, 22, 44, 58, 22, 32, 18, 36, 10, %  7
  32, 15, 22, 24, 42, 19, 29, 00, 30, 30, 16, 10, 16, 21, 11, 59, 36, 61, 45, %  8
  92, 22, 10, 36, 57, 53, 36, 30, 00, 69, 28, 27, 33, 50, 28, 21, 19, 69, 11, %  9
  61, 22, 41, 36, 57, 53, 36, 30, 69, 00, 28, 27, 33, 50, 28, 21, 19, 69, 11, % 10
  31, 36, 26, 22, 16, 44, 22, 16, 28, 28, 00, 33, 42, 39, 37, 30, 27, 28, 25, % 11
  22, 33, 52, 22, 15, 33, 22, 10, 27, 27, 33, 00, 38, 36, 38, 26, 21, 27, 21, % 12
  31, 36, 21, 22, 21, 39, 44, 16, 33, 33, 42, 38, 00, 39, 42, 57, 27, 33, 25, % 13
  55, 38, 24, 22, 69, 61, 58, 21, 50, 50, 39, 36, 39, 00, 34, 27, 32, 50, 22, % 14
  26, 36, 21, 22, 16, 34, 22, 11, 28, 28, 37, 38, 42, 34, 00, 30, 22, 28, 25, % 15
  19, 24, 33, 10, 33, 27, 32, 59, 21, 21, 30, 26, 57, 27, 30, 00, 39, 52, 37, % 16
  21, 26, 33, 13, 31, 30, 18, 36, 19, 19, 27, 21, 27, 32, 22, 39, 00, 19, 34, % 17
  61, 22, 10, 36, 57, 53, 36, 61, 69, 69, 28, 27, 33, 50, 28, 52, 19, 00, 11, % 18
  14, 55, 33, 10, 23, 22, 10, 45, 11, 11, 25, 21, 25, 22, 25, 37, 34, 11, 00  % 19
]);


output [
  "max_score: " ++ show(max_score) ++ "\n" ++
  "total: " ++ show(total) ++ "\n" ++
  "x: " ++ show(x) ++ "\n" ++
  "z: " ++ show(z) ++ "\n" ++
  "s: " ++ show(s) ++ "\n"

] ++ ["\n"];
