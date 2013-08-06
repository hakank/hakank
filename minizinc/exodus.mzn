% 
% Exodus puzzle (Dell Logic Puzzles) in MiniZinc.
% 
% From 
% http://brownbuffalo.sourceforge.net/ExodusClues.html
% """
% Title: Exodus
% Author: Sophy McHannot
% Publication: Dell Logic Puzzles
% Issue: April, 1998
% Page: 14
% Stars: 2

% In preparation for Passover, five children at Hebrew school 
% (Bernice,Carl,Debby,Sammy, and Ted) 
% have been chosen to present
% different parts of the story of the Exodus from Egypt 
%  (burning bush, captivity,
%   Moses's youth, Passover, or the Ten Commandments). 
% Each child is a different age 
%   (three, five, seven, eight, or ten), 
% and the family of each child has recently made its own exodus 
% to America from a different country 
% (Ethiopia, Kazakhstan, Lithuania, Morocco, or Yemen). 
% Can you find the age of each child, his or her family's country of 
% origin, and the part of the Exodus story each related?

%  1. Debby's family is from Lithuania.
%  2. The child who told the story of the Passover is two years older
%     than Bernice.
%  3. The child whose family is from Yemen is younger than the child from
%     the Ethiopian family.
%  4. The child from the Moroccan family is three years older than Ted.
%  5. Sammy is three years older than the child who told the story of
%     Moses's youth in the house of the Pharaoh.
%  6. Carl related the story of the captivity of the Israelites in Egypt.
%  7. The five-year-old child told the story of the Ten Commandments.
%  8. The child who told the story of the burning bush is either two or
%     three years older than the one whose family came from
%     Kazakhstan.
%
% Determine: Age -- Child -- Country -- Story
% """
%
% Compare with the following models:
% * ECLiPSE: http://www.hakank.org/eclipse/exodus.ecl
% * SICStus Prolog: http://www.hakank.org/sicstus/exodus.pl
% * Gecode: http://www.hakank.org/gecode/exodus.cpp

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 
int: n = 5;

int: Bernice = 1;
int: Carl = 2;
int: Debby = 3;
int: Sammy = 4;
int: Ted = 5;

set of int: R = 1..n;
var R: BurningBush;
var R: Captivity;
var R: MosessYouth;
var R: Passover;
var R: TenCommandments;
array[R] of var R: Story = 
                 [BurningBush, Captivity, MosessYouth, Passover, TenCommandments];

array[R] of var {3,5,7,8,10}: Age;

var R: Ethiopia;
var R: Kazakhstan;
var R: Lithuania;
var R: Morocco;
var R: Yemen;
array[R] of var R: Country = 
    [Ethiopia, Kazakhstan, Lithuania, Morocco, Yemen];


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
    alldifferent(Story) /\
    alldifferent(Age) /\
    alldifferent(Country) /\

    Debby = Lithuania /\
    Age[Passover] = Age[Bernice] + 2 /\
    Age[Yemen] < Age[Ethiopia] /\
    Age[Morocco] = Age[Ted] + 3 /\
    Age[Sammy] = Age[MosessYouth] + 3 /\
    Carl = Captivity /\
    Age[TenCommandments] = 5 /\
    ( 
        (Age[BurningBush] = Age[Kazakhstan] + 2)
    \/
        (Age[BurningBush] = Age[Kazakhstan] + 3)
    )
;


output
[
  "Story: " ++ show(Story) ++ "\n" ++
  "Age: " ++ show(Age) ++ "\n" ++
  "Country: " ++ show(Country) ++ "\n"
];
