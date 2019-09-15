/*

  Euler problem 19 in SWI Prolog

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
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).

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


%
% http://en.wikipedia.org/wiki/Julian_day
% gregorian date -> julian day
date2julian(Year,Month,Day, JD) :-
  A is floor((14-Month) / 12), % 1 for Jan or Feb, 0 for other months
  Y is Year + 4800 - A,
  M is Month + 12*A - 3, % 0 for Mars, 11 for Feb
  JD is Day + floor( (153*M + 2) / 5) + 365*Y + floor(Y/4) -
       floor(Y / 100) + floor(Y / 400) - 32045.


% julian day -> gregorian date
julian2date(JD, Date) :-
  Y is 4716,
  V is 3,
  J is 1401,
  U is 5,
  M is 2,
  S is 153,
  N is 12,
  W is 2,
  R is 4,
  B is 274277,
  P is 1461,
  C is  -38,
  F is JD + J + (((4 * JD + B) div 146097) * 3) div 4 + C,
  E is R * F + V,
  G is mod(E, P) div R,
  H is U * G + W,
  Day is (mod(H, S)) div U + 1,
  Month is mod(H div S + M, N) + 1,
  Year is (E div P) - Y + (N + M - Month) div N,
  Date = [Year,Month,Day].
