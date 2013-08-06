%
% Stable Marriage in Minizinc
%
% Translation of the OPL version from
% Pascal Van Hentenryck "The OPL Optimization Programming Language"
% E.g.
% http://www.comp.rgu.ac.uk/staff/ha/ZCSP/additional_problems/stable_marriage/stable_marriage.pdf
%

%
% Minizinc model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

int: num_women; % antal women
int: num_men; % antal men

array[1..num_women, 1..num_men] of var 1..num_men: rankWomen;
array[1..num_men, 1..num_women] of var 1..num_women: rankMen;

array[1..num_men] of var 1..num_women: wife;
array[1..num_women] of var 1..num_men: husband;


solve satisfy;

constraint
        forall(m in 1..num_men) ( 
            husband[wife[m]] = m 
        )  
        /\        
        forall(w in 1..num_women) ( 
           wife[husband[w]] = w 
        )  
        /\
        forall(m in 1..num_men, o in 1..num_women) (
           rankMen[m,o] < rankMen[m, wife[m]] ->
           rankWomen[o,husband[o]] < rankWomen[o,m]
        )
        /\
        forall(w in 1..num_women, o in 1..num_men) (
           rankWomen[w,o] < rankWomen[w,husband[w]] ->
           rankMen[o,wife[o]] < rankMen[o,w]
        )
%       /\
%       inverse(wife,husband)
;

output [
       "wife   : ", show(wife), "\n",
       "husband: ", show(husband), "\n",
]
;

%
% data from van Hentenryck
%
% %%% Men = {Richard,James,John,Hugh,Greg};
% num_men = 5;
% %%% Women = {Helen,Tracy,Linda,Sally,Wanda};
% num_women = 5;
% rankWomen = 
% [|1, 2, 4, 3, 5,
%  |3, 5, 1, 2, 4,
%  |5, 4, 2, 1, 3,
%  |1, 3, 5, 4, 2,
%  |4, 2, 3, 5, 1
%  |];
% rankMen =
% [|5, 1, 2, 4, 3,
%  |4, 1, 3, 2, 5,
%  |5, 3, 2, 4, 1,
%  |1, 5, 4, 3, 2,
%  |4, 3, 2, 1, 5
%  |];



% Data from
% http://mathworld.wolfram.com/StableMarriageProblem.html
% """
% In the rankings illustrated above, the male-optimal stable 
% marriage is 
%     4, 2, 6, 5, 3, 1, 7, 9, 8, 
% and the female-optimal stable marriage is 
%     1, 2, 8, 9, 3, 4, 7, 6, 5. 
% A stable marriage can be found using StableMarriage[m, w] in the 
% Mathematica package Combinatorica` (which can be loaded with the 
% command <<Combinatorica`)
%
% 
% Note that the matrices given at the MathWorld page are transposed.
% 
% There are 6 solutions, but none is the solution given above:
%
% wife   : [6, 1, 4, 8, 5, 7, 3, 2, 9]
% husband: [2, 8, 7, 3, 5, 1, 6, 4, 9]
%
% wife   : [6, 1, 4, 8, 5, 9, 3, 2, 7]
% husband: [2, 8, 7, 3, 5, 1, 9, 4, 6]
%
% wife   : [6, 4, 1, 8, 5, 7, 3, 2, 9]
% husband: [3, 8, 7, 2, 5, 1, 6, 4, 9]
%
% wife   : [6, 4, 9, 8, 3, 7, 1, 5, 2]
% husband: [7, 9, 5, 2, 8, 1, 6, 4, 3]
%
% wife   : [6, 5, 9, 8, 3, 7, 1, 4, 2]
% husband: [7, 9, 5, 8, 2, 1, 6, 4, 3]
%
% wife   : [7, 5, 9, 8, 3, 6, 1, 4, 2]
% husband: [7, 9, 5, 8, 2, 6, 1, 4, 3]


num_men = 9;
num_women = 9;

% The transposed version from the MathWorld page
% rankMen =
% [|7, 5, 4, 9, 2, 2, 1, 5, 6,
%  |3, 4, 8, 7, 6, 7, 6, 6, 1,
%  |8, 8, 3, 4, 4, 8, 2, 9, 4,
%  |9, 3, 9, 2, 9, 6, 3, 1, 7,
%  |6, 1, 7, 5, 8, 5, 8, 2, 5, 
%  |4, 2, 5, 8, 7, 3, 5, 8, 8, 
%  |2, 6, 6, 3, 5, 4, 4, 4, 3,
%  |1, 7, 1, 1, 1, 1, 9, 3, 9, 
%  |5, 9, 2, 6, 3, 9, 7, 7, 2
%  |];

rankMen = 
[|7, 3, 8, 9, 6, 4, 2, 1, 5,
 |5, 4, 8, 3, 1, 2, 6, 7, 9,
 |4, 8, 3, 9, 7, 5, 6, 1, 2,
 |9, 7, 4, 2, 5, 8, 3, 1, 6,
 |2, 6, 4, 9, 8, 7, 5, 1, 3,
 |2, 7, 8, 6, 5, 3, 4, 1, 9,
 |1, 6, 2, 3, 8, 5, 4, 9, 7,
 |5, 6, 9, 1, 2, 8, 4, 3, 7,
 |6, 1, 4, 7, 5, 8, 3, 9, 2,
 |];


% The transposed version from the MathWorld page
% rankWomen =
% [|3, 9, 3, 8, 6, 2, 9, 6, 8,
%  |1, 4, 1, 7, 9, 4, 3, 3, 2,
%  |5, 8, 8, 5, 2, 5, 8, 2, 6,
%  |2, 1, 9, 3, 5, 1, 2, 1, 4,
%  |8, 7, 5, 2, 1, 6, 7, 8, 9,
%  |7, 6, 4, 6, 4, 8, 5, 4, 1,
%  |6, 3, 2, 4, 7, 3, 4, 5, 3,
%  |9, 2, 6, 9, 3, 9, 6, 9, 7,
%  |4, 5, 7, 1, 8, 7, 1, 7, 5
%  |];

rankWomen =
[|3, 1, 5, 2, 8, 7, 6, 9, 4,
 |9, 4, 8, 1, 7, 6, 3, 2, 5,
 |3, 1, 8, 9, 5, 4, 2, 6, 7,
 |8, 7, 5, 3, 2, 6, 4, 9, 1,
 |6, 9, 2, 5, 1, 4, 7, 3, 8,
 |2, 4, 5, 1, 6, 8, 3, 9, 7,
 |9, 3, 8, 2, 7, 5, 4, 6, 1,
 |6, 3, 2, 1, 8, 4, 5, 9, 7,
 |8, 2, 6, 4, 9, 1, 3, 7, 5
 |];

% From 
% http://www.csee.wvu.edu/~ksmani/courses/fa01/random/lecnotes/lecture5.pdf
% num_men = 4;
% num_women = 4;
% rankMen = 
% [|1,2,3,4,
%  |2,1,3,4,
%  |1,4,3,2,
%  |4,3,1,2
%  |];

% rankWomen =
% [|1,2,3,4,
%  |4,3,2,1,
%  |1,2,3,4,
%  |3,4,1,2
%  |];
