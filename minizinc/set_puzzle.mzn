% 
% Set puzzle in MiniZinc.
% 
%
% http://www.setgame.com/set/puzzle_frame.htm
%
% http://en.wikipedia.org/wiki/Set_(game)
% """
% Different "categories":
%   * They all have the same number , or they have three different numbers.
%   * They all have the same symbol , or they have three different symbols.
%   * They all have the same shading, or they have three different shadings.
%   * They all have the same color  , or they have three different colors.
% """
%
% Benjamin Lent Davis and Diane Maclagan, The Card Game Set:
%  http://www.math.rutgers.edu/~maclagan/papers/set.pdf
%
% 
% From http://www.nytimes.com/ref/crosswords/setpuzzle.html#howtoplay
% [my codings in parenthesis]
% """
% A 'Set' is 3 cards in which each individual feature is either all the 
% SAME on each card... OR all DIFFERENT on each card.
%  
%  Symbols: Each card has   ovals (1), squiggles (2), or diamonds (3) on it;
%  Color  : The symbols are red   (1), green     (2), or purple   (3);
%  Number : Each card has   one   (1), two       (2), or three    (3)  symbols on it;
%  Shading: The symbols are solid (1), striped   (2), or open     (3).
% """

% Solution Basic 1 puzzle
% x = [{1..3}#(3), {2,4,5}#(3), {3,5,8}#(3), {6,8,9}#(3)]

% Solution Basic 2 puzzle
% x = [{1,2,9}#(3), {2,3,5}#(3), {2,4,8}#(3), {4,7,9}#(3)]


%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

int: n; % = 9; % number of items
% int: n = 12; % number of items 

int: num_sets; % = 4; % number of Sets to find 
% int: num_sets = 5; % number of Sets to find 

int: m = 3; % number of items in a Set

int: num_flavours = 3; % number of flavours of the attributes

int: num_attributes = 4; % number of different attributes in each item
% assumption: all the attributes has exact m "flavours".
array[1..n, 1..num_attributes] of var 1..num_flavours: puzzle;

% decision variable: the sets to find
array[1..num_sets] of var set of 1..n: x;

% solve satisfy;
solve :: set_search(x, input_order, indomain_min, complete) satisfy;

constraint

   % exact three items in each set
   forall(i in 1..num_sets) (
     card(x[i]) = m
   )
   /\ % must be different sets
   all_different(x)
   /\
   forall(i in 1..num_sets) (

      % identify the items in this set  
      forall(a in 1..num_attributes) (
        let {
           array[1..m] of var 1..n: arr
        }
        in 
        % assign the set
        forall(j in 1..m) (
          arr[j] in x[i]
        )
        /\ % symmetry breaking: all different and sorted
        forall(j in 1..m-1) (
          arr[j] < arr[j+1]
        )
        /\
        (  
           % EITHER all are same with regard to this attribute
           forall(j in 1..m-1) (
                puzzle[arr[j],a] = puzzle[arr[j+1],a]
             )
           \/ 
           % OR all are different with regard to this attribute 
           forall(i, j in 1..m where i != j) (
              puzzle[arr[i],a] != puzzle[arr[j],a]
           )
        )
      ) % end forall 1..num_attributes

   ) % end forall 1..num_sets

   /\ % Symmetry breaking: order the sets
      % Don't works with Gecode/fz 3.3.1 or ECLiPSE/ic (version 6.0 #146)
   decreasing(x)    
;

output [
  show(x) ++ "\n"
]
 ++ [
  if j = 1 then "\n" else " " endif ++
    show(puzzle[i,j])
  | i in 1..n, j in 1..num_attributes
]; 

%
% data
%

% Basic 1 puzzle from 
% http://www.nytimes.com/ref/crosswords/setpuzzle.html#howtoplay
%
% Number of items: 9. Sets to find: 4
% n = 9;
% num_sets = 4;
% puzzle = 
% array2d(1..n, 1..num_attributes, [
%          1,2,3,1, % 1 oval       green  3 solid
%          2,2,3,2, % 2 squiggles  green  3 striped
%          3,2,3,3, % 3 diamond    green  3 open
%          1,2,3,3, % 4 oval       green  3 open
%          3,2,3,1, % 5 diamond    green  3 solid
%          2,3,3,3, % 6 squiggles  purple 3 open
%          3,3,3,1, % 7 diamond    purple 3 solid
%          3,2,3,2, % 8 diamond    green  3 striped
%          1,1,3,1  % 9 oval       red    3 solid
% ]);

% Basic 2 puzzle from
% http://www.nytimes.com/ref/crosswords/setpuzzle.html#howtoplay
% n = 9;
% num_sets = 4;
% puzzle = 
%  array2d(1..n, 1..num_attributes, [
%        3,1,1,1, % 1  diamond   red    1  solid
%        3,2,3,1, % 2  diamond   green  3  solid 
%        1,2,3,1, % 3  oval      green  3  solid 
%        2,3,1,1, % 4  squiggles purple 1  solid
%        2,2,3,1, % 5  squiggles green  3  solid
%        2,2,1,1, % 6  squiggles green  1  solid
%        1,3,3,1, % 7  oval      purple 3  solid
%        1,1,2,1, % 8  oval      red    2  solid
%        3,3,2,1  % 9  diamond   purple 2  solid
% ]);


% Another puzzle from 
%% http://www.nytimes.com/ref/crosswords/setpuzzle.html
%n = 9;
%num_sets = 4;
%puzzle = 
% array2d(1..n, 1..num_attributes, [
%1,2,2,3,
%1,2,3,2,
%2,2,2,3,
%1,2,3,3,
%1,2,3,1,
%3,2,2,3,
%2,2,1,1,
%2,2,3,1,
%2,2,3,2,
%]);

%
% This puzzle is from http://www.anvandbart.se/set-soduko-fluga
% Solution: [{1,2,4}#(3), {2,5,6}#(3), {3,4,9}#(3), {6,8,9}#(3)]
% n = 9;
% num_sets = 4;
% puzzle = array2d(1..n, 1..num_attributes, 
%  [2,1,2,1,
%   3,3,2,1,
%   1,2,2,2,
%   1,2,2,1,
%   3,2,2,1,
%   3,1,2,1,
%   3,2,2,2,
%   2,3,2,2,
%   1,2,2,3,
% ]);


%
% From "The Card Game Set" (see above), page 2, figure 2
% Mumber of items: 12 Sets to find: 5
% Solution: 
% [{1..3}#(3), {1,5,9}#(3), {2,6,10}#(3), {3,7,11}#(3), {4,8,12}#(3)]
% 1    oval       red     1 solid
% 2    squiggles  green   2 striped
% 3    diamond    purple  3 open
% 4    oval       purple  1 striped
% 5    squiggles  red     1 solid
% 6    diamond    purple  2 striped
% 7    oval       red     3 solid
% 8    squiggles  red     2 open
% 9    diamond    red     1 solid
% 10   oval       red     2 striped
% 11   squiggles  green   3 striped
% 12   diamond    green   3 solid
% note: change n to 12, and num_sets to 5;
n = 12;
num_sets = 5;
puzzle = 
array2d(1..n, 1..num_attributes, 
[
1,1,1,1,
2,2,2,2,
3,3,3,3,
1,3,1,2,
2,1,1,1,
3,3,2,2,
1,1,3,1,
2,1,2,3,
3,1,1,1,
1,1,2,2,
2,2,3,2,
3,2,3,1,
]);
