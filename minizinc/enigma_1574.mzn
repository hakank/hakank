% 
% Enigma Number 1574 (Doubly square dates) in MiniZinc.
% 
% From New Scientist
% http://www.newscientist.com/article/mg20427370.400-enigma-number-1574.html
% """
% 02 December 2009 by Richard England
%
% Doubly square dates
% 
% 4 April 2001 was the first doubly square date of the century because 
% whether written in the order day.month.year or in the order month.day.year 
% (in each instance with two digits for each element) it comes out as 
% 04.04.01, and 40401 is 201^2.
% 
% Still with two digits for each element, there are some doubly square 
% dates for which the square that comes from the order day.month.year 
% is not the same as the square that comes from the order month.day.year. 
% This is the case for each of the next two doubly square dates after 
% 4 April 2001. What are those two dates (in the same form as 4 April 2001)?
% """

% The two dates are:
%
%  2009-04-12 and 2009-12-04, i.e
%  12 April 2009 and 4 December 2009
%


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 

array[1..6] of var 0..99: day_month_year_a;
array[1..6] of var 0..99: month_day_year_a;
var int: day_month_year;
var int: month_day_year;
var 1..31: day;
var 1..12: month;
var 0..99: year;

var int: y1; % ;
var int: y2; %;

% Convert array <-> number in base 10
predicate toNum(array[int] of var 0..9: a, var int: n) =
          let { int: len = length(a) }
          in
          n = sum(i in 1..len) (
            ceil(pow(10.0, int2float(len-i))) * a[i]
          )
;


% solve satisfy;
solve :: int_search(
           day_month_year_a ++ 
           month_day_year_a,
           first_fail, 
           indomain_max, 
           complete) 
       satisfy;

constraint
   toNum([day_month_year_a[i] | i in 1..2], day)   /\
   toNum([day_month_year_a[i] | i in 3..4], month) /\
   toNum([day_month_year_a[i] | i in 5..6], year)  /\

   toNum([month_day_year_a[i] | i in 1..2], month) /\
   toNum([month_day_year_a[i] | i in 3..4], day)   /\
   toNum([month_day_year_a[i] | i in 5..6], year)  /\

   toNum(month_day_year_a, month_day_year) /\
   toNum(day_month_year_a, day_month_year)    

   /\ % we don't allow month == day
   month != day

   /\
   y1 > 0 /\ y2 > 0 /\

   % check the squares
   y1*y1 = month_day_year /\
   y2*y2 = day_month_year

;


output
[
  show([year, month, day])  
];
