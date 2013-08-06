% 
% Babysitting puzzle (Dell Logic Puzzles) in MiniZinc.
% 
% Problem from http://brownbuffalo.sourceforge.net/BabysittingClues.html
% """
% Title: Babysitting
% Author: Scott Marley
% Publication: Dell Logic Puzzles
% Issue: April, 1998
% Page: 7
% Stars: 1
%
% Each weekday, Bonnie takes care of five of the neighbors' children. 
% The children's names are Keith, Libby, Margo, Nora, and Otto; last names 
% are Fell, Gant, Hall, Ivey, and Jule. Each is a different number of years 
% old, from two to six. Can you find each child's full name and age?
%
% 1. One child is named Libby Jule.
% 2. Keith is one year older than the Ivey child, who is one year older 
%    than Nora.
% 3. The Fell child is three years older than Margo.
% 4. Otto is twice as many years old as the Hall child.
%
% Determine: First name - Last name - Age 
% """

% Compare with the F1 model 
% http://www.f1compiler.com/samples/Babysitting.f1.html

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% Result
%        Keith Libby Margo Nora Otto
%        Fell  Jule  Hall  Gant Ivey
%        5     6     2     3    4  
% last = [1, 4, 3, 5, 2]
% age = [5, 6, 2, 3, 4]



include "globals.mzn"; 
int: n = 5;
set of int: r = 1..n;

r: Keith = 1;
r: Libby = 2;
r: Margo = 3;
r: Nora = 4;
r: Otto = 5;
array[r] of int: first =  [Keith, Libby, Margo, Nora, Otto];

array[r] of var 2..6: age;

var r: Fell;
var r: Gant;
var r: Hall;
var r: Ivey;
var r: Jule;
array[r] of var r: last = [Fell, Gant, Hall, Ivey, Jule];


solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint
    all_different(last)
    /\
    all_different(age)

   /\ %  1. One child is named Libby Jule.
   Jule = Libby

   /\ %  2. Keith is one year older than the Ivey child, who is one 
      %     year older than Nora.
   age[Keith] = age[Ivey] + 1 /\
   Keith != Ivey
   
   /\
   age[Ivey] = age[Nora] + 1 /\
   Ivey != Nora

   /\ %  3. The Fell child is three years older than Margo.
   age[Fell] = age[Margo] + 3 /\
   Fell != Margo 

   /\ %  4. Otto is twice as many years old as the Hall child.
   age[Otto] = age[Hall]*2 /\
   Otto != Hall

;

output
[
  "first: " ++ show(first) ++ "\n" ++
  "last: " ++ show(last) ++ "\n" ++
  "age: " ++ show(age)
];