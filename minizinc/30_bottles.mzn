/* 

  30 bottles problem in MiniZinc.

  From Alcuins 
  via Paul Vaderlind "Klassisk Nöjesmatematik", 2003, page 38.
  This book is in Swedish (English: "Classical Recreational Mathematics")
  """
  A man died and left 30 bottles to his 3 sons. 10 bottles was filled
  with oil, 10 was half full with oil, and 10 was empty. The wish of the
  man was that all the sons should get the same amount of bottles and
  the same amount of oil. How to distribute bottles and oil in a fair
  way if it's not allowed to pour oil from one bottle to another.
  How many solutions are there?
  """

  Here are the five solutions:
    [2, 6, 2]
    [3, 4, 3]
    [5, 0, 5]
    ----------
    [2, 6, 2]
    [4, 2, 4]
    [4, 2, 4]
    ----------
    [3, 4, 3]
    [3, 4, 3]
    [4, 2, 4]
    ----------
    [0, 10, 0]
    [5, 0, 5]
    [5, 0, 5]
    ----------
    [1, 8, 1]
    [4, 2, 4]
    [5, 0, 5]
    ----------
    ==========



  This model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my MiniZinc page: http://www.hakank.org/minizinc/

*/


include "globals.mzn"; 

int: n; % number of different bottle types
array[int] of int: b; % number of bottles for each bottle type
array[int] of int: t; % ration of oil for each bottle type (integers)
int: num_sons; % number of sons

int: tot_oil = sum([t[i]*b[i] | i in 1..n]); % total amount of oil
int: tot_bottles = sum(b);                   % total number of bottles

% decision variables
% How many bottles of each type should be distributed to each son
array[1..num_sons,1..n] of var 0..tot_oil: x;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  forall(s in 1..num_sons) (
    % total number of bottles per son:
    %    number of bottles for this son = total number of bottle / number of sons
    % I have a habit of converting division to multiplication if possible
    %
    % sum([x[s,j] | j in 1..n]) = tot_bottles / num_sons /\ % This works 
    num_sons*sum(x[s,..] ) = tot_bottles /\

    
    % total amount of oil per son
    % sum([x[s,j]*t[j] | j in 1..n]) = tot_oil / num_sons % This works as well
    num_sons*sum([x[s,j]*t[j] | j in 1..n]) = tot_oil
    
    /\ % symmetry breaking
    if s < num_sons then
       lex_lesseq(x[s,..],x[s+1,..])
    endif
  )
  /\ % ensure that the exact the number of bottles are distributed for each bottle type
  forall(j in 1..n) (
    sum(x[..,j]) = b[j]
  )
;

output
[ "tot_bottles: \(tot_bottles)\n",
  "tot_oil: \(tot_oil)\n",
] ++
[
        "\(x[i,..])\n"
        | i in 1..num_sons
];


%
% Original problem (5 solutions, see above)
%
n = 3; % number of bottle types
b = [10,10,10]; % number of bottles of each type
t = [2,1,0]; %  number of [filled, half filled, empty] bottles. [1,1/2,0] -> [2,1,0]
num_sons = 3;


/*
  Vaderlind (op cit, page 40):
  """
  Problem 15 
  How to distribute 5 full, 8 half-full, and 11 empty bottles of wine
  between three persons if each person get the same number of bottles 
  and the same amout of wine. Find all solutions.
  """
  
  Here the solutions:
    [1, 4, 3]
    [2, 2, 4]
    [2, 2, 4]
    ----------
    [1, 4, 3]
    [1, 4, 3]
    [3, 0, 5]
    ----------
    [0, 6, 2]
    [2, 2, 4]
    [3, 0, 5]
    ----------
    ==========


*/

% n = 3; % number of bottle types
% b = [5,8,11]; % number of bottles of each type
% t = [2,1,0]; %  number of [filled, half filled, empty] bottles. [1,1/2,0] -> [2,1,0]
% num_sons = 3;


% And some additional problems (Vaderlind, op.cit, page 215):
% """
% Also, try to solve a similar puzzle, this time with 
%   5 full, 11 half-full, and 8 empty bottles (three solutions)
% and
%  11 full, 5 half-full, and 8 empty bottles (one solution)
% """
% n = 3; % number of bottle types
% b = [5,11,8]; % number of bottles of each type
% t = [2,1,0]; %  number of [filled, half filled, empty] bottles. [1,1/2,0] -> [2,1,0]
% num_sons = 3;


% n = 3; % number of bottle types
% b = [11,5,8]; % number of bottles of each type
% t = [2,1,0]; %  number of [filled, half filled, empty] bottles. [1,1/2,0] -> [2,1,0]
% num_sons = 3;


/*
  Random instance (found by my Picat model 30_bottles.pi).
  There are 12 solutions

    [3, 4, 1, 1, 1, 0]
    [3, 4, 1, 1, 1, 0]
    [4, 0, 5, 0, 0, 1]
    ----------
    [3, 3, 3, 1, 0, 0]
    [3, 3, 3, 1, 0, 0]
    [4, 2, 1, 0, 2, 1]
    ----------
    ...
    ----------
    [3, 3, 3, 0, 1, 0]
    [3, 4, 1, 2, 0, 0]
    [4, 1, 3, 0, 1, 1]
    ----------
    ==========


 */
% n = 6;
% t = [0,5,7,9,9,10];
% b = [10,8,7,2,2,1];
% num_sons = 3;
 



/*
  Vaderlind, op.cit. page 40
  """
  Problem 16

  How can 9 containers which contains 1,2,3,4,5,6,7,8,and 9 liter wine be 
  distributed between three persons such that each person get three containers
  and the same amount of wine. Determine all solutions. It is not allowed
  to pour over wine from one container to another
  """

  Here are the two solutions:

  [0, 0, 1, 0, 1, 0, 1, 0, 0]
  [0, 1, 0, 1, 0, 0, 0, 0, 1]
  [1, 0, 0, 0, 0, 1, 0, 1, 0]
  ----------
  [0, 0, 1, 1, 0, 0, 0, 1, 0]
  [0, 1, 0, 0, 0, 1, 1, 0, 0]
  [1, 0, 0, 0, 1, 0, 0, 0, 1]
  ----------
  ==========

  I.e. 
  a) p1: 3, 5, 7
     p2: 2, 4, 9
     p3: 1, 6, 8

  b) p1: 3, 4, 8
     p2: 2, 6, 7
     p3: 1, 5, 9

  (At page 215, Vaderlind notices that these two solutions are 
   magic squares of order 3.)

 */
% n = 9; % number of bottle types
% b = [1,1,1,1,1,1,1,1,1]; % number of bottles of each type
% t = [1,2,3,4,5,6,7,8,9]; %  ratio of each bottle type
% num_sons = 3;
