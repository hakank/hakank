% 
% A card trick, 1 in MiniZinc.
% 
% From Peter Cameron's blog post
% "A card trick, 1"
% http://cameroncounts.wordpress.com/2011/08/20/a-card-trick-1/
% """
% Diamond Geezer's card puzzle 
%   [ http://diamondgeezer.blogspot.com/2011/08/diamond-puzzle.html
%    (a graeco latin square problem, cf
%     http://hakank.org/minizinc/latin_square_card_puzzle.mzn
%    )
%   ] 
% reminded me of a more complicated puzzle, which was devised 
% and solved by my colleague and co-author Donald Preece. He is very 
% proud of this, and justifiably so. In this post I will simply set 
% the problem; later I will describe the mathematical background, 
% and maybe give the solution.
%
% The problem is to take a regular pack of 52 playing cards, and lay them out in a 
% rectangle with four rows of 13 cards, so as to satisfy the following conditions:
%
%  - Each suit is represented once in each column.
%  - Each face value is represented once in each row.
%  - Two cards with the same value never occur together in a column.
%  - Each row contains four cards of one suit and three of each of the others.
%  - Any two columns share eactly one value, and any two values occur together in 
%    exactly one column.
%
% Some brief commentary. Condition 4 says that the suits are as near as 
% possible to being balanced over the rows; since 4 doesn't divide 13, we can't 
% have exactly the same number of cards of each suit in a row. This condition could 
% also be stated in the equivalent form "Each suit has four cards in one row and 
% three in each of the others."
%
% Condition 5 is in some sense the hardest to satisfy. You may want to try 
% to solve this condition, concentrating just on the values and the columns, and 
% worry about the suits and the positions of the cards in the columns later.
% """
% 

% Also, see the following blog posts by Peter Cameron:
% "A card trick, 2"
% http://cameroncounts.wordpress.com/2011/08/22/a-card-trick-2/
% "A card trick, 3"
% http://cameroncounts.wordpress.com/2011/08/22/a-card-trick-3/
% "A card trick, 4"
% http://cameroncounts.wordpress.com/2011/08/23/a-card-trick-4/
%
% Also, more about this can be found, for example, in Donald Preece's
% "Combinatorial Designs",
% http://www.maths.qmul.ac.uk/~fjw/goldsmiths/2009/DAP/combin.pdf
%

%
% In "A card trick, 4"
% http://cameroncounts.wordpress.com/2011/08/23/a-card-trick-4/
% Peter Cameron writes this:
%
% [hakank: I removed the fancy symbols and changed "10" to "T" for 
%  more compact output]
%
% """
% Here is Donald Preece's solution to his problem, taken from his paper 'Some partly 
% cyclic 13x4 Youden "squares" and a balanced arrangement for a pack of cards', 
% in Utilitas Math. 22 (1982), 255-263. (I hope I haven't made any typing mistakes!)
%
% AS JC QH KD 5S 6S 7S 2D 3C 4H 8D 9C TH
% 2H AH 6C TS QD JD 4D 9H 8H KS 7C 3S 5C
% 3D 8S AD 7H 2C KC QC JS TD 9D 6H 5H 4S
% 4C 5D 9S AC KH 3H JH TC QS 8C 2S 7D 6D
% """



% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: num_suites = 4;
int: num_values = 13;
int: rows = num_suites;
int: columns = num_values;

array[1..num_values] of string: values_s = ["A","2","3","4","5","6","7","8","9","T","J","Q","K"];
array[1..num_suites] of string: suites_s = ["H","S","C","D"];


% decision variables
array[1..rows, 1..columns] of var 1..num_suites: suites;
array[1..rows, 1..columns] of var 1..num_values: values;

% the values in columns as set representation
array[1..columns] of var set of 1..num_values: columns_set;


% solve satisfy;
solve :: int_search(
        [suites[i,j] | i in 1..rows, j in 1..columns] 
        ++
        [values[i,j] | i in 1..rows, j in 1..columns] 
        ,
        occurrence, 
        indomain_min, 
        complete) 
    satisfy;


constraint
   % ensure 4 occurrence of each value
   forall(v in 1..num_values) (
      count([values[i,j] | i in 1..rows, j in 1..columns], v, num_suites)
   ) 

   /\ % ensure 13 occurrence of each suites
   forall(v in 1..num_suites) (
      count([suites[i,j] | i in 1..rows, j in 1..columns], v, num_values)
   ) 
   
   /\ % All 52 cards are distinct 
   alldifferent([1+13*(suites[i,j]-1)+(values[i,j]-1) | i in 1..rows, j in 1..columns] ) % :: domain

   % /\
   % forall(k in 1..rows*columns) (
   %    % exists(i in 1..rows, j in 1..columns) (
   %    let {
   %    var 1..rows: i,
   %    var 1..columns: j
   %     } in
   %       k = 1+13*(suites[i,j]-1)+(values[i,j]-1)
   %   % )
   % )
;


% The problem specific constraints
constraint

   % "Each suit is represented once in each column"
   forall(j in 1..columns) (
       alldifferent([suites[i,j] | i in 1..rows])
   )

   /\ % "Each face value is represented once in each row"
   forall(i in 1..rows) (
       alldifferent([values[i,j] | j in 1..columns])
   )

   /\ % "Two cards with the same value never occur together in a column"
   forall(j in 1..columns) (
       alldifferent([values[i,j] | i in 1..rows])
   )

   /\ % "Each row contains four cards of one suit and three of each of the others."
   forall(i in 1..rows) (
      let {
         array[1..rows] of var num_suites-1..num_suites: gcc
      } 
      in 
      global_cardinality([suites[i,j] | j in 1..columns],set2array(index_set(gcc)), gcc)
   )

   /\ % Channel set version and matrix version of column values.
   forall(j in 1..columns) (
        forall(v in 1..num_values) (
           v in columns_set[j] <-> exists(i in 1..rows) ( values[i,j] = v)
        )
   )

   /\ % "Any two columns share eactly one value, ..."
   forall(j1, j2 in 1..columns where j1 < j2) (
       card(columns_set[j1] intersect columns_set[j2]) = 1
   )

   /\ % "... and any two values occur together in exactly one column."
   forall(v1, v2 in 1..num_values where v1 < v2) (
      1 = sum(j in 1..columns) ( bool2int(v1 in columns_set[j] /\ v2 in columns_set[j]) )
   )

   % /\ % symmetry breaking
   % values[1,1] = 1
   % /\
   % suites[1,1] = 1

;

output 
[
   "suites:", show(suites), "\n",
   "values:", show(values), "\n"
]
++
[
    if j = 1 then "\n" else "" endif ++
      show(suites_s[fix(suites[i,j])]) ++ "" ++ show(values_s[fix(values[i,j])]) ++ " " 
   | i in 1..rows, j in 1..columns
]
 ++ ["\n"]
;

