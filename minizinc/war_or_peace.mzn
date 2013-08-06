% 
% War or Peace problem in MiniZinc.
% 
% From the Alma0 model war_or_peace.a0  
% http://www.cwi.nl/en/alma
% """
% There are N countries.
% Each pair of two countries is either at war or has a peace treaty.
% Each pair of two countries that has a common enemy has a peace treaty.
% What is the minimum no of peace treaties?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Note: 
% There are 35 solutions with the minimum number of peace treaties 12.
% 

% include "globals.mzn"; 

int: war = 0; 
int: peace = 1; 
int: N = 8;
% int: currentbound = (N-1)*N DIV 2;

array[1..N, 1..N] of var war..peace: x;
var int: countpeaces = sum(i,j in 1..N) (bool2int(x[i,j] = peace));
solve :: int_search(
        [x[i,j] | i,j in 1..N], 
        first_fail, % "occurrence", 
        indomain_min,
        complete) 
        minimize countpeaces;
        % satisfy;

constraint
  % countpeaces = 12 /\
  forall(i in 1..N-1) (
    forall(j in i+1..N) (
      (
        x[i,j] = war
        /\
        forall(k in 1..i-1) ( (x[k,i] = peace) \/ (x[k,j] = peace) )
      )
      \/
      (x[i,j] = peace)
    )
  )
;


output 
[ "\ncountpeaces: ", show(countpeaces)] ++
[
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i,j in 1..N 
] ++ ["\n"];