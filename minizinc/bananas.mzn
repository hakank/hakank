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
  % 3*bananas/5 + 5*oranges/7 + 7*mangoes/9 + 9*apples/3 = 100
  % 3*bananas div 5 + 5*oranges div 7 + 7*mangoes div 9 + 9*apples div 3 = 100

  int2float(3*bananas)/5.0 + int2float(5*oranges)/7.0 + 
            int2float(7*mangoes)/9.0 + int2float(9*apples)/3.0 = 100.0

  /\
  bananas + oranges + mangoes + apples = 100
;

output
[ 
  "the_sum: " ++ show(the_sum) ++ "\n" ++
  show([bananas, oranges, mangoes, apples])
];