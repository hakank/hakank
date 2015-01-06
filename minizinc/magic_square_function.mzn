% 
% Magic square using functions in MiniZinc 2.0.
% 
% Some functions in MiniZinc 2.0 for syntax sugaring.
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 4;

%
% extract the i'th row of matrix a
%
function array[int] of var int: row(array[int,int] of var int: a, int: i) =
    [a[i,j] | j in index_set_2of2(a)]
;

%
% extract the j'th row of matrix a
%
function array[int] of var int: col(array[int,int] of var int: a, int: j) =
    [a[i,j] | i in index_set_1of2(a)]
;

%
% extract the first diagonal of square matrix a
%
function array[int] of var int: diagonal1(array[int,int] of var int: a) =
   [a[i,i] | i in index_set_1of2(a)]
;

%
% extract the second diagonal of square matrix a
%
function array[int] of var int: diagonal2(array[int,int] of var int: a) =
   let {
     int: len = card(index_set_1of2(a)),
     array[index_set_1of2(a)] of var int: d = [a[i,len-i+1]|i in 1..len];    
   } in d
;

%
% flatten a 2d matrix
%
function array[int] of var int: flatten(array[int,int] of var int: a) =
    array1d(a)
;

% nicer output of an array (slice)
% function string: nice1d(array[int] of var int: a) =
%    join(" ", [show_int(3,a[i]) | i in index_set(a)])
% ;


% nice output of a matrix
function string: nice2d(array[int,int] of var int: a) =
   join("\n", 
       [
         join(" ", [show_int(3,a[i,j]) | i in index_set_1of2(a)])
       | j in index_set_2of2(a)
       ]
      )
;


%
% decision variables
%
array[1..n,1..n] of var 1..n*n: x;
var 1..pow(n,3): s;



% solve satisfy;
solve :: int_search(flatten(x), first_fail, indomain_median, complete) satisfy;

constraint
  all_different(flatten(x)) /\
  forall(i in 1..n) (
    s = sum(row(x,i)) /\
    s = sum(col(x,i))
  )
  /\
  sum(diagonal1(x)) = s /\
  sum(diagonal2(x)) = s

  /\ s = ( n * (n*n + 1)) div 2
;

output ["s: \(s)\n\(nice2d(x))"];



