%
% Magic squares on Frénicle standard form in MiniZinc
%
% From
% http://en.wikipedia.org/wiki/Fr%C3%A9nicle_standard_form
% """
% A magic square is in Frénicle standard form, named for 
% Bernard Frénicle de Bessy, if the following two conditions apply:
%  - the element at position [1,1] (top left corner) is the smallest 
%    of the four corner elements; and
%  - the element at position [1,2] (top edge, second from left) is 
%    smaller than the element in [2,1].
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


include "globals.mzn";

int: n = 4;
% var 1..n*n*n: total;
int: total = ( n * (n*n + 1)) div 2;

array[1..n,1..n] of var 1..n*n: magic;


solve satisfy;
% solve :: int_search(
%         [magic[i,j] | i, j in 1..n], 
%         input_order,
%         indomain_min,
%         complete) 
%     satisfy;

constraint
        all_different([magic[i,j] | i in 1..n, j in 1..n]) :: domain
        /\
        forall(k in 1..n) (
           sum(i in 1..n) (magic[k,i]) = total
           /\
           sum(i in 1..n) (magic[i,k]) = total
        )
       /\ % diagonal
       sum(i in 1..n) (magic[i,i]) = total
       /\ % diagonal
       sum(i in 1..n) (magic[i,n-i+1]) = total
;

%
% symmetry breaking
%
% Activating all these constraints we get the
% "standard" way of counting the number of solutions:
%    1, 0, 1, 880, 275305224
% i.e. this sequence: http://oeis.org/A006052
%
% Without the constraints the number of solutions are:
%  N  #solutions
%  -------------
%  1     1
%  2     0
%  3     8
%  4  7040
%  5  many... 
%
constraint
  minimum(magic[1,1], [magic[1,1], magic[1,n], magic[n,1], magic[n,n]])
  /\
  magic[1,2] < magic[2,1]
;


output [
  "Total: " ++ show(total) ++ "\n"
] ++
[
    %   show(magic)
      show(magic[i,j]) ++ " " ++ if j mod n == 0 then "\n" else "" endif 
      | i,j in 1..n
];
