/*

  Euler problem 19 in SICStus Prolog

  """
  You are given the following information, but you may prefer 
  to do some research for yourself.

  * 1 Jan 1900 was a Monday.
  * Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.
  * A leap year occurs on any year evenly divisible by 4, but not 
    on a century unless it is divisible by 400.
  
  How many Sundays fell on the first of the month during the 
  twentieth century (1 Jan 1901 to 31 Dec 2000)?
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus_prolog/

*/

:- ensure_loaded(hakank_utils).

go :-
        L = [
             euler19a
            ],
        run_problems(L).
  
%%
%% 0.1s 
%%
euler19a :-
        date2julian(1901, 1, 1,DateFrom),
        date2julian(2000,12,31,DateTo),
        findall(Date,
                between(DateFrom,DateTo,Date),
                L),
        e19a(L,0,Sum),
        writeln(Sum).

e19a([],Sum,Sum).
e19a([D|T],Sum0,Sum) :-
        julian2date(D, DD),
        [DD1,DD2,DD3] = DD,
        dow(DD1,DD2,DD3,Dow),
        (  
           (DD3 == 1,  Dow == 0)
        ->
           Sum1 is Sum0 + 1
        ;
           Sum1 is Sum0
        ),
        e19a(T,Sum1,Sum).


%
% Day of week, Sakamoto's method
% http:%en.wikipedia.org/wiki/Weekday_determination#Sakamoto.27s_Method
%
dow(Y, M, D, Dow) :-
   T = [0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4],
   (M < 3
   ->
    YY is Y - 1
   ;
    YY is Y
   ),
   nth1(M,T,TM),
   Dow is (YY + YY div 4 - YY div 100 + YY div 400 + TM + D) mod 7.
