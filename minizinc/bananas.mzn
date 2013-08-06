% 
% Bananas problem in MiniZinc.
%
% """
% In three dollars, you get 5 bananas, in five dollars, 7 oranges, in
% seven dollars, 9 mangoes and in nine dollars, three apples, I need to
% purchase 100 fruits in 100 dollars. Please keep in mind that all type
% of fruits need to be purchased but I do not like banana and apple, so
% these should be of minimum quantity.
% """
% Note: I have forgot where I got this problem.

var 0..100: bananas;
var 0..100: oranges;
var 0..100: mangoes;
var 0..100: apples;


var int: the_sum = bananas+apples;
solve minimize the_sum;

constraint
  bananas >= 1 /\ oranges >= 1 /\ mangoes >= 1 /\ apples >= 1 
  /\
  % 5*bananas/3 + 7*oranges/5 + 9*mangoes/7 + 3*apples/9 = 100
  % 5*bananas div 3 + 7*oranges div 5 + 9*mangoes div 7 + 3*apples div 9 = 100
  int2float(5*bananas)/3.0 + int2float(7*oranges)/5.0 + 
            int2float(9*mangoes)/7.0 + int2float(3*apples)/9.0 = 100.0
  /\
  bananas + oranges + mangoes + apples = 100
;

output
[ 
  show([bananas, oranges, mangoes, apples])
];